;;; based on
;;; https://www.bepuentertainment.com/blog/2022/3/10/seeking-the-tootbird

(in-package #:org.shirakumo.fraf.trial.sttb)

(defconstant STTB-ITERATIONS 64)
(defconstant STTB-TOLERANCE 0.0001)

(defun update-tootbird (s0 dir tootbird best)
  (declare (type point s0)
           (type vec3 dir tootbird)
           (type single-float best)
           (optimize speed))
  ;; update TOOTBIRD with distance BEST for new point S0 in
  ;; (normalized) direction DIR, return new distance (or BEST if
  ;; unchanged) and flag indicating if we are done.

  ;; calculate signed distance from origin to plane
  (let ((d (v. s0 dir)))
    (cond
      ;; If negative, origin is outside Minkowski difference.
      #++ ;; version for calculating distance between non-intersecting objects
      ((minusp d)
       ;; If we are trying to calculate distance between
       ;; non-intersecting objects, original description suggests just
       ;; continuing as before, but that doesn't seem to terminate
       ;; well with this implementation? Alternately, it suggests
       ;; switching to seeking origin and using a different
       ;; termination test. Haven't implemented a different test yet,
       ;; so that doesn't terminate well either
       ;; set tootbird to origin, and best distance to 0
       (vzero tootbird)
       (values 0.0 0))
      ((minusp d)
       ;; since we don't handle termination correctly for
       ;; non-intersecting case yet, just return "no hit"
       (values 0.0 -1))
      ;; if distance is 0, origin is the new point, which is on
      ;; surface of Minkowski difference, so we are done
      ((< (abs d) (expt STTB-TOLERANCE 2))
       (values 0.0 1))
      ;; if new distance is closer than best tootbird, project origin
      ;; onto the plane to get new tootbird
      ((< d best)
       (!v* tootbird dir d)
       (values d 0))
      (T
       ;; no change, keep old value
       (values best 0)))))

(defun update-simplex (dim s0 s1 s2 s3 tootbird seeking-origin)
  ;; returns (values new-dim done-flag)
  (declare (type point s0 s1 s2 s3)
           (type vec3 tootbird)
           (type (or T NIL) seeking-origin)
           (type (unsigned-byte 2) dim)
           (optimize speed))
  (let ((ret 0))
    (case dim
      ;; for 0 through 2 point simplexes, just add the new point, unless
      ;; it is too close to an existing point
      (0
       (p<- s1 s0)
       (setf dim 1))
      (1
       (cond
         ((v~= s0 s1)
          (when seeking-origin
            ;; got same point twice in a row while searching for
            ;; origin, we are done
            (setf ret -1)))
         (T
          (p<- s2 s0)
          (setf dim 2))))
      (2
       (cond
         ((or (v~= s0 s1) (v~= s0 s2))
          (when seeking-origin
            (setf ret -1)))
         (T
          (p<- s3 s0)
          (setf dim 3))))
      ;; If we already have 3 points, try to pick the new triangle
      ;; closest to containing the (projection of the) tootbird.
      ;; First, find normal of a plane perpendicular to the old
      ;; triangle, and passing through the new point and one of the
      ;; old points. Use that to determine which side of each of those
      ;; planes contains the tootbird. If it isn't on the same side of
      ;; all 3 planes, we can directly determine which point to
      ;; replace. If it is on the same side as all 3, replace
      ;; whichever has the lowest distance (original just picked an
      ;; arbitrary point to replace, but this seems to work better?).
      (3
       (cond
         ;; if same as an existing point, just ignore it (possibly
         ;; should replace this with 'if it is close to existing
         ;; point, replace it'?)
         ((or (v= s0 s1) (v= s0 s2) (v= s0 s3))
          (when seeking-origin
            ;; if we repeated a point, we are done. (We frequently get
            ;; longer loops though, so this isn't enough of a test to
            ;; make origin-seeking actually work by itself.)
            (setf ret -1)))
         (T
          (let* ((ab (v- s2 s1))
                 (ac (v- s3 s1))
                 (an (v- s0 s1))
                 (bn (v- s0 s2))
                 (cn (v- s0 s3))
                 (tn (v- s0 tootbird))
                 (n (vec3)))
            (declare (dynamic-extent ab ac an bn cn n))
            ;; normal of old triangle
            (!vc n ac ab)
            ;; normals of planes through new point
            (nvc an n)
            (nvc bn n)
            (nvc cn n)
            ;; determine which side of each of those planes tootbird
            ;; is on
            (let* ((da (v. tn an))
                   (db (v. tn bn))
                   (dc (v. tn cn))
                   (f (logior (if (plusp da) 1 0)
                              (if (plusp db) 2 0)
                              (if (plusp dc) 4 0))))
              (case f
                ((#b001 #b101) ;; in front of a, behind b, replace c
                 (p<- s3 s0))
                ((#b010 #b011) ;; in front of b, behind c, replace a
                 (p<- s1 s0))
                ((#b100 #b110) ;; in front of c, behind a, replace b
                 (p<- s2 s0))
                ;; ambiguous, pick lowest distance (signed, so -2 is
                ;; lower than -1)
                ((#b000 #b111)
                 (if (< da db)
                     (if (< dc da)
                         (p<- s3 s0)
                         (p<- s1 s0))
                     (if (< db dc)
                         (if (< da dc)
                             (p<- s1 s0)
                             (p<- s2 s0))
                         (p<- s3 s0)))))))))))
    (values dim ret)))

;;; triangle case based on "Improving the GJK algorithm for faster and
;;; more reliable distance queries between convex objects." by Mattia
;;; Montanari, Nik Petrinic, and Ettore Barbieri. 2017.
;;; ACM Trans. Graph. 36, 3, Article 30 (June 2017)
;;; DOI: http://dx.doi.org/10.1145/3083724
(defun nearest-point-on-simplex (dim s1 s2 s3 target dest)
  ;; Find point on simplex S1[,S2[,S3]] (with DIM points) nearest to
  ;; TARGET, and return it in DEST. Updates S1[,S2[,S3]] to contain
  ;; new simplex supporting that point, and returns size of new
  ;; simplex.
  (declare (type point s1 s2 s3)
           (type vec3 dest target)
           (type (unsigned-byte 2) dim)
           (optimize speed))

  (flet ((line (a b d)
           (let* ((m (v- b a))
                  (dt (v- target a))
                  (mm (v. m m)))
             (declare (dynamic-extent m dt))
             (cond
               ((< mm 0.00000001)
                ;; degenerate, treat as 1 point (keep newer, in hopes it
                ;; works better next pass)
                (p<- a b)
                (v<- d a)
                1)
               (T
                (let* ((t0 (/ (v. m dt) mm)))
                  (cond
                    ((<= t0 0)
                     (v<- d a)
                     1)
                    ((<= 1 t0)
                     (v<- d b)
                     (p<- a b)
                     1)
                    (T
                     (!v+* d a m t0)
                     2))))))))

    (case dim
      (1
       ;; if only 1 point, that is closest point
       (v<- dest s1))
      (2
       (setf dim (line s1 s2 dest)))
      (3
       (let* ((ab (v- s2 s1))
              (ac (v- s3 s1))
              (at (v- target s1))
              (n (vec3))
              (l^2 0.0)
              (p0 (vec3))
              (umax 0.0)
              (j -1)
              (flat NIL)
              (epsilon 0.00001))
         (declare (dynamic-extent ab ac at n p0)
                  (type single-float umax))
         (!vc n ac ab)
         (setf l^2 (vsqrlength n))
         ;; project tootbird onto plane of simplex (if possible)
         (cond
           ((< l^2 (expt epsilon 2))
            (setf flat T))
           (T
            (!v* p0 n (/ (v. at n) l^2))
            (!v- p0 target p0)))

         (flet ((flat (edges)
                  (let ((d MOST-POSITIVE-SINGLE-FLOAT)
                        ;; best result seen so far
                        (best-dim 0)
                        (best-point (vec3))
                        (b0 (point))
                        (b1 (point))
                        ;; temp space used by each LINE call
                        (point (vec3))
                        (c0 (point))
                        (c1 (point)))
                    (declare (dynamic-extent best-point
                                             b0 b1
                                             point c0 c1))
                    (loop for j from 0 to 2
                          when (logbitp j edges)
                            do (p<- c0 (if (= j 0) s2 s1))
                               (p<- c1 (if (< j 2) s3 s2))
                               (let ((r (line c0 c1 point))
                                     (d* (vsqrdistance point target)))
                                 (declare (type (unsigned-byte 4) r))
                                 (when (< d* d)
                                   (setf d d*
                                         best-dim r)
                                   (v<- best-point point)
                                   (p<- b0 c0)
                                   (when (< 1 r) (p<- b1 c1)))))
                    ;; should always find a solution
                    (assert (/= d MOST-POSITIVE-SINGLE-FLOAT))
                    ;; copy results to output
                    (v<- dest best-point)
                    (p<- s1 b0)
                    (when (< 1 best-dim) (p<- s2 b1))
                    (setf dim best-dim))))
           (cond
             ;; if too flat, just try all edges and pick the best
             ;; result from those
             (flat
              (flat #b111))
             ;; normal case
             (T
              ;; pick axis-aligned plane with largest projection of
              ;; simplex
              (loop for i below 3
                    for u of-type single-float = (+ (projected-cross s1 s2 i)
                                                    (projected-cross s2 s3 i)
                                                    (projected-cross s3 s1 i))
                    when (< (abs umax) (abs u))
                      do (setf umax u
                               j i))
              ;; and project simplex and point onto that plane, and
              ;; find the support
              (flet ((projected-cross (a b x)
                       (macrolet ((c (x y)
                                    `(- (* (,x a) (,y b))
                                        (* (,y a) (,x b)))))
                         (ecase x
                           (0 (c vy vz))
                           (1 (- (c vx vz)))
                           (2 (c vx vy))))))
                (let* ((c3 (+ (projected-cross p0 s1 j)
                              (projected-cross s1 s2 j)
                              (projected-cross s2 p0 j)))
                       (c1 (+ (projected-cross p0 s2 j)
                              (projected-cross s2 s3 j)
                              (projected-cross s3 p0 j)))
                       (c2 (+ (projected-cross p0 s3 j)
                              (projected-cross s3 s1 j)
                              (projected-cross s1 p0 j)))
                       ;; figure out which values have same sign as
                       ;; umax (0 for either counts as not matching,
                       ;; and we will have to check the edge)
                       (flags (cond
                                ((plusp umax)
                                 (logior (if (plusp c1) 0 1)
                                         (if (plusp c2) 0 2)
                                         (if (plusp c3) 0 4)))
                                ((minusp umax)
                                 (logior (if (minusp c1) 0 1)
                                         (if (minusp c2) 0 2)
                                         (if (minusp c3) 0 4)
                                         ))
                                (T #b111)))
                       (in (= flags 0)))
                  (declare (dynamic-extent c1 c2 c3))
                  (cond
                    ;; support is entire triangle
                    (in
                     (!v* dest s1 (/ c1 umax))
                     (!v+* dest dest s2 (/ c2 umax))
                     (!v+* dest dest s3 (/ c3 umax))
                     3)
                    ;; otherwise, try edges and pick best
                    (T
                     (flat flags))))))))))))
  dim)

(defun update-dir (dim s1 s2 s3 dir tootbird seeking-origin)
  ;; find next search direction and determine if we are done. Returns
  ;; (values new-dim done-flag), updates simplex in S1-S3, and updates
  ;; DIR.
  (declare (type point s1 s2 s3)
           (type vec3 dir tootbird)
           (type (or T NIL) seeking-origin)
           ;; don't have a good termination test for this case yet, so
           ;; not actually used.
           (ignorable seeking-origin)
           (type (unsigned-byte 2) dim)
           (optimize speed))
  ;; find point on simplex closest to tootbird, and store it in DIR
  ;; (and update simplex to subset that supports that point)
  (setf dim (nearest-point-on-simplex dim s1 s2 s3 tootbird dir))
  ;; is tootbird on simplex?
  (let ((dist (vsqrdistance dir tootbird)))
    (cond
      ;; if tootbird is on simplex, we are done, and simplex is
      ;; face/edge/point of difference (locally) closest to origin
      ((and
        ;; coarse test against fixed distance
        (< dist 1e-4)
        ;; then test against epsilon based on size of simplex
        (< dist (* 2 SINGLE-FLOAT-EPSILON
                   (ecase dim
                     (1 (vinorm s1))
                     (2 (+ (vinorm s1) (vinorm s2)))
                     (3 (+ (vinorm s1) (vinorm s2) (vinorm s3)))))))
       ;; return new simplex and that we are done
       (values dim 1))
      (T
       ;; otherwise update dir as direction from closest point to
       ;; tootbird
       (!v- dir tootbird dir)
       (nvunit dir)
       ;; return new simplex size and that we aren't done
       (values dim 0)))))

(defun %sttb (a b hit)
  (let ((s0 (point))
        (s1 (point))
        (s2 (point))
        (s3 (point))
        (dir (vec3))
        (dim 0)
        (ret 0)
        (tootbird (vec3))
        (best-distance MOST-POSITIVE-SINGLE-FLOAT))
    (declare (dynamic-extent s0 s1 s2 s3 dir tootbird)
             (type (unsigned-byte 2) dim)
             (type (signed-byte 2) ret)
             (type single-float best-distance)
             (optimize speed))
    ;; website suggests "direction from center of Minkowski difference
    ;; to origin" as first direction, so approximate that with
    ;; difference of centers
    (trial:global-location a dir)
    (trial:global-location b s0)
    (!v- dir dir s0)
    (nvunit dir)
    (when (v~= dir 0.0)
      ;; no initial guess for best direction, just pick something
      (v<- dir +vx3+))
    (loop for i below STTB-ITERATIONS
          do (search-point s0 dir a b)
             (setf (values best-distance ret)
                   (update-tootbird s0 dir tootbird best-distance))
          while (zerop ret)
          do (setf dim
                   (update-simplex dim s0 s1 s2 s3 tootbird
                                   (zerop best-distance)))
             (setf (values dim ret)
                   (update-dir dim s1 s2 s3 dir tootbird
                               (zerop best-distance)))
          while (zerop ret))

    (let ((b-point (vec3))
          (a-point (vec3))
          (p (vec3)))
      (declare (dynamic-extent a-point b-point p))
      ;; calculate hit location from tootbird and simplex
      (ecase dim
        (0
         (v<- a-point (point-a s0))
         (v<- b-point (point-b s0)))
        (1
         (v<- a-point (point-a s1))
         (v<- b-point (point-b s1)))
        (2
         (let* ((ds (v- s2 s1))
                (dt (v- tootbird s1))
                (l (vsqrlength ds)))
           (declare (dynamic-extent ds dt))
           (cond
             ((< l 0.000001)
              ;; simplex is small, use the midpoint of smaller edge
              ;; on original shapes as hit location, and tootbird as
              ;; normal
              (let ((p1 (+ (vsqrdistance (point-a s1) (point-a s2))))
                    (p2 (+ (vsqrdistance (point-b s1) (point-b s2)))))
                (cond
                  ((<= p1 p2)
                   (!v+ a-point (point-a s1) (point-a s2))
                   (nv* a-point 0.5f0)
                   (!v+ b-point a-point tootbird))
                  (T
                   (!v+ b-point (point-b s1) (point-b s2))
                   (nv* b-point 0.5f0)
                   (!v- a-point b-point tootbird)))))
             (T
              (setf l (/ (v. ds dt) l))
              (!v- ds (point-a s2) (point-a s1))
              (!v+* a-point (point-a s1) ds l)
              (!v- ds (point-b s2) (point-b s1))
              (!v+* b-point (point-b s1) ds l)))))
        (3
         (cond
           ((barycentric s1 s2 s3
                         ;; not sure if this should be looking for
                         ;; point close to tootbird or origin?
                         #++ tootbird (vec3)
                         p)
            (nv+* a-point (point-a s1) (vx p))
            (nv+* a-point (point-a s2) (vy p))
            (nv+* a-point (point-a s3) (vz p))
            (nv+* b-point (point-b s1) (vx p))
            (nv+* b-point (point-b s2) (vy p))
            (nv+* b-point (point-b s3) (vz p)))
           (T
            ;; barycentric failed (not sure if this still happens
            ;; with it using doubles?). Use tootbird for normal,
            ;; then pick whichever shape had smaller simplex, and
            ;; use middle of that simplex as hit location. Using
            ;; perimeter rather than area since it is pretty common
            ;; for one of the shapes to be supported by a single
            ;; edge, so 0 area doesn't indicate smaller region.
            (let ((p1 (+ (vdistance (point-a s1) (point-a s2))
                         (vdistance (point-a s2) (point-a s3))
                         (vdistance (point-a s3) (point-a s1))))
                  (p2 (+ (vdistance (point-b s1) (point-b s2))
                         (vdistance (point-b s2) (point-b s3))
                         (vdistance (point-b s3) (point-b s1)))))
              (v<- (trial:hit-normal hit) tootbird)
              (cond
                ((<= p1 p2)
                 (!v+ a-point (point-a s1) (point-a s2))
                 (nv+ a-point (point-a s3))
                 (nv* a-point 0.33333334f0)
                 (!v+ b-point a-point tootbird))
                (T
                 (!v+ b-point (point-b s1) (point-b s2))
                 (nv+ b-point (point-b s3))
                 (nv* b-point 0.33333333f0)
                 (!v- a-point b-point tootbird))))))))
      (v<- (trial:hit-location hit) a-point)
      (v<- (trial:hit-normal hit) b-point)
      (nv- (trial:hit-normal hit) a-point)
      (setf (trial:hit-depth hit) (vlength (trial:hit-normal hit)))
      (if (= 0.0 (trial:hit-depth hit))
          (v<- (trial:hit-normal hit) +vy3+)
          (nv/ (trial:hit-normal hit) (trial:hit-depth hit))))
    (plusp ret)))

(defun detect-hits (a b hits start end)
  (declare (type trial:primitive a b))
  (declare (type (unsigned-byte 32) start end))
  (declare (type simple-vector hits))
  (declare (optimize speed))

  (when (<= end start)
    (return-from detect-hits start))
  (let ((hit (aref hits start)))
    (prog1
        (cond ((%sttb a b hit)
               (trial:finish-hit hit a b)
               (1+ start))
              (T
               start)))))

(setf trial:+generic-hit-detector+ #'detect-hits)
