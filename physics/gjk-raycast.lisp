;;; The raycast implementation is largely based on "Ray Casting
;;; against General Convex Objects with Application to Continuous
;;; Collision Detection" by Gino Van Den Bergen available at
;;; http://dtecta.com/papers/jgt04raycast.pdf

;;; Raycasting uses an alternative implementation of GJK originally
;;; based on "Improving the GJK algorithm for faster and more reliable
;;; distance queries between convex objects." by Mattia Montanari, Nik
;;; Petrinic, and Ettore Barbieri. 2017.  ACM Trans. Graph. 36, 3,
;;; Article 30 (June 2017) DOI: http://dx.doi.org/10.1145/3083724

;;; Probably could be made to share more code with the main GJK
;;; implementation, but would probably slow down the normal path, so
;;; didn't bother trying. In particular, the raycast algorithm depends
;;; on knowing the nearest point on the current simplex, rather than
;;; just the direction to that point.

(in-package #:org.shirakumo.fraf.trial.gjk)

;; todo: define a separate POINT type to use for raycast, to avoid
;; extra copies of unused B slot in P<-
(declaim (inline pcopy))
(defun pcopy (p)
  (p<- (point) p))

(defun signed-volumes (dim s0 s1 s2 s3 dir epsilon)
  ;; returns # of dimensions remaining, and any remaining points in
  ;; s1,s2,s3 (unless 3-simplex contains origin in which case 4 is
  ;; returned and s0-3 are unchanged). also returns point on simplex
  ;; nearest to origin in DIR
  (ecase dim
    (1
     (p<- s1 s0)
     (v<- dir s1)
     1)
    (2 (sv1d s0 s1 s2 s3 dir epsilon))
    (3 (sv2d s0 s1 s2 s3 dir epsilon))
    (4 (sv3d s0 s1 s2 s3 dir epsilon))))

(declaim (inline sv-compare-signs))
(defun sv-compare-signs (a b)
  (or (and (< a 0) (< b 0))
      (and (< 0 a) (< 0 b))))

(defun sv3d (s0 s1 s2 s3 dir epsilon)
  (declare (type point s0 s1 s2 s3)
           (type vec3 dir)
           (type single-float epsilon)
           (optimize speed))
  (let* ((m (mat4 (vx s0) (vx s1) (vx s2) (vx s3)
                  (vy s0) (vy s1) (vy s2) (vy s3)
                  (vz s0) (vz s1) (vz s2) (vz s3)
                  1 1 1 1))
         (c (vec4))
         (mdet 0f0)
         (flat NIL)
         (signs 0))
    (declare (dynamic-extent m c)
             (type (unsigned-byte 8) signs)
             (type single-float mdet))
    (loop for j below 4
          for cj = (mcofactor m 3 j)
          do (setf (vref c j) cj)
             (incf mdet cj))
    (setf flat (< (abs mdet) epsilon))
    (loop for i below 4
          do (setf (ldb (byte 1 i) signs)
                   (if (sv-compare-signs mdet (aref (varr c) i))
                       1 0)))
    (cond
      ;; contained in simplex
      ((and (not flat) (= signs #b1111))
       (!v* dir s0 (/ (vx c) mdet))
       (!v+* dir dir s1 (/ (vy c) mdet))
       (!v+* dir dir s2 (/ (vz c) mdet))
       (!v+* dir dir s3 (/ (vw c) mdet))
       4)
      ;; best face is previous simplex?
      ((= signs #b1110)
       ;; not sure exactly what is happening in this case, but pretty
       ;; sure it means we are not making progress and terminating
       ;; seems to work fairly well, so doing that for now. Possibly
       ;; should return DIR unmodified, or calculated only from face 0
       ;; instead?
       (!v* dir s0 (/ (vx c) mdet))
       (!v+* dir dir s1 (/ (vy c) mdet))
       (!v+* dir dir s2 (/ (vz c) mdet))
       (!v+* dir dir s3 (/ (vw c) mdet))
       4)
      ;; otherwise find best of the faces
      (T
       (let (;;
             (d MOST-POSITIVE-SINGLE-FLOAT)
             ;; best result seen so far
             (best-dim 0)
             (best-dir (vec3))
             (b0 (point))
             (b1 (point))
             (b2 (point))
             ;; s* passed to particular call of sv2d
             (cdir (vec3))
             (c0 (point))
             (c1 (point))
             (c2 (point))
             (c3 (point)))
         (declare (dynamic-extent best-dir
                                  b0 b1 b2
                                  cdir c0 c1 c2 c3))
         (loop for j from 1 to 3
               when (or flat (not (logbitp j signs)))
                 do (p<- c0 s0)
                    (p<- c1 (if (< j 2) s2 s1))
                    (p<- c2 (if (< j 3) s3 s2))
                    (let ((r (sv2d c0 c1 c2 c3 cdir epsilon))
                          (d* (vsqrlength cdir)))
                      (declare (type (unsigned-byte 4) r))
                      (when (< d* d)
                        (setf d d*
                              best-dim r)
                        (v<- best-dir cdir)
                        ;; results are in c1[,c2[,c3]]
                        (p<- b0 c1)
                        (when (> r 1) (p<- b1 c2))
                        (when (> r 2) (p<- b2 c3)))))
         ;; should always find a solution
         (assert (/= d MOST-POSITIVE-SINGLE-FLOAT)
                 nil "Couldn't find best face?~% flat=~s mdet=~s c=~s~% ~s~%"
                 flat mdet (vcopy c)
                 (loop for j from 1 to 3
                       collect (or flat
                                   (not (sv-compare-signs mdet (vref c j))))))
         ;; copy results to output
         (v<- dir best-dir)
         (p<- s1 b0)
         (when (> best-dim 1) (p<- s2 b1))
         (when (> best-dim 2) (p<- s3 b2))
         best-dim)))))

(declaim (inline projected-cross v2c))
(defun projected-cross (a b x)
  ;; project A,B onto plane normal to axis with index X, then return
  ;; X'th component of resulting vectors
  (macrolet ((c (x y)
               `(- (* (,x a) (,y b))
                   (* (,y a) (,x b)))))
    (ecase x
      (0 (c vy vz))
      (1 (- (c vx vz)))
      (2 (c vx vy)))))

(defun v2c (a b)
  (- (* (vx a) (vy b))
     (* (vy a) (vx b))))

(defun sv2d (s0 s1 s2 s3 dir epsilon)
  (declare (type point s0 s1 s2 s3)
           (type vec3 dir)
           (type single-float epsilon)
           (optimize speed))
  (let* ((ab (v- s1 s0))
         (ac (v- s2 s0))
         (bc (v- s2 s1))
         ;; vc doesn't get DX yet?
         ;;(n (vc ac ab))
         ;;(l² (vsqrlength n))
         (n (vec3))
         (l^2 0.0)
         (p0 (vec3))
         (umax 0.0)
         (j -1)
         (flat NIL))
    (declare (dynamic-extent ab ac bc n p0)
             (type single-float umax))
    (!vc n ac ab)
    (setf l^2 (vsqrlength n))
    ;; project origin onto plane (if possible)
    (if (< l^2 (expt epsilon 2))
        (setf flat T)
        (!v* p0 n (/ (v. s0 n) l^2)))
    (unless flat
      ;; pick axis-aligned plane with largest projection
      (loop for i below 3
            for u of-type single-float = (projected-cross ab ac i)
            when (> (abs u) (abs umax))
              do (setf umax u
                       j i))
      (setf flat (< (abs umax) epsilon)))

    ;; if too flat, just pick best result from 1d test against all edges
    (when flat
      (let ((d MOST-POSITIVE-SINGLE-FLOAT)
            ;; best result seen so far
            (best-dim 0)
            (best-dir (vec3))
            (b0 (point))
            (b1 (point))
            ;; s* passed to particular call of sv1d
            (cdir (vec3))
            (c0 (point))
            (c1 (point))
            (c2 (point))
            (c3 (point)))
        (declare (dynamic-extent best-dir
                                 b0 b1
                                 cdir c0 c1 c2 c3))
        (loop for j from 0 to 2
              do (p<- c0 (if (= j 0) s1 s0))
                 (p<- c1 (if (< j 2) s2 s1))
                 (let ((r (sv1d c0 c1 c2 c3 cdir epsilon))
                       (d* (vsqrlength cdir)))
                   (declare (type (unsigned-byte 4) r))
                   (when (< d* d)
                     (setf d d*
                           best-dim r)
                     (v<- best-dir cdir)
                     ;; results are in c1[,c2]
                     (p<- b0 c1)
                     (when (> r 1) (p<- b1 c2)))))
        ;; should always find a solution
        (assert (/= d MOST-POSITIVE-SINGLE-FLOAT))
        ;; copy results to output
        (v<- dir best-dir)
        (p<- s1 b0)
        (when (> best-dim 1) (p<- s2 b1))
        (return-from sv2d best-dim)))

    ;; calculate barycentric coordinates on that plane (seems slightly
    ;; more accurate than calling BARYCENTRIC, and not sure that
    ;; handles points outside the triangle the way we want?
    (let* ((ap (v- p0 s0))
           (pb (v- p0 s1))
           (pc (v- p0 s2))
           (sabc (projected-cross ab ac j))
           (s (if (minusp sabc) -1 1))
           (abc (abs sabc))
           (t1 (* s (projected-cross pb pc j)))
           (n1 (<= t1 0))
           (t2 (* s (projected-cross ap ac j)))
           (n2 (<= t2 0))
           (t3 (* s (projected-cross ab ap j)))
           (n3 (<= t3 0))
           (out (+ (if n1 1 0) (if n2 1 0) (if n3 1 0))))
      (declare (dynamic-extent ap pb pc))
      (cond
        ;; support is entire triangle
        ((and (zerop out)
              (< t1 abc)
              (< t2 abc)
              (< t3 abc))
         ;; possibly should recalculate one of the coords from the
         ;; other 2, but not sure which.
         (!v* dir s0 (/ t1 abc))
         (!v+* dir dir s1 (/ t2 abc))
         (!v+* dir dir s2 (/ t3 abc))
         (p<- s3 s2)
         (p<- s2 s1)
         (p<- s1 s0)
         3)
        ((= out 1)
         ;; support is line opposite negative λ
         (flet ((edge (a b m)
                  (let* ((t0 (- (/ (v. m a)
                                   (v. m m)))))
                    (!v+* dir a m t0))
                  (p<- s2 b)
                  (p<- s1 a)
                  2))
           (cond
             (n1 (edge s1 s2 bc))
             (n2 (edge s0 s2 ac))
             (n3 (edge s0 s1 ab)))))
        ((= out 2)
         ;; support is point with positive λ
         (cond
           ((not n1)
            (v<- dir s0)
            (p<- s1 s0)
            1)
           ((not n2)
            (v<- dir s1)
            (p<- s1 s1)
            1)
           ((not n3)
            (v<- dir s2)
            (p<- s1 s2)
            1)))
        (T (error "shouldn't get here?"))))))

(defun sv1d (s0 s1 s2 s3 dir epsilon)
  (declare (type point s0 s1 s2 s3)
           (type vec3 dir)
           (type single-float epsilon)
           (optimize speed))
  (declare (ignore s3))
  (let* ((m (v- s1 s0))
         (mm (v. m m)))
    (declare (dynamic-extent m))
    (cond
      ((< (abs mm) epsilon)
       ;; degenerate segment, return either point (s1 to avoid a copy)
       (v<- dir s1)
       1)
      (T
       (let* ((t0 (- (/ (v. m s0)
                        mm))))
         (cond
           ((<= t0 0)
            ;; start point (keep s0 in s1)
            (v<- dir s0)
            (p<- s1 s0)
            1)
           ((<= 1 t0)
            ;; end point (keep s1 in s1)
            (v<- dir s1)
            1)
           (T
            ;; keep both points (in s1,s2)
            (!v+* dir s0 m t0)
            (p<- s2 s1)
            (p<- s1 s0)
            2)))))))

(trial:define-ray-test trial:primitive ()
  (let* (;; inputs
         (s ray-location)
         (r ray-direction)
         ;; state
         (tt 0.0)
         (x (vcopy s))
         (w (vec3))
         (p (vec3))
         (v (vcopy x))
         ;; set P
         (dim 0) ;; # of valid elements in P
         (s0 (point))
         (s1 (point))
         (s2 (point))
         (s3 (point))
         ;; stuff for alternative termination tests
         (stuck 0)
         (last-updated 0)
         ;; used to scale tolerance for termination test relative to simplex
         (maxdist 1.0)
         ;; paper says "order of magnitude larger than machine
         ;; epsilon" (* 6 single-float-negative-epsilon) seems like a
         ;; good balance between quality of results and chance of
         ;; getting stuck in a loop (8 is about half as likely as 6 to
         ;; loop, 5 is slightly more likely to loop, 4 is about 2x, 3
         ;; is ~4x, 2 is ~7x).
         ;; we mostly detect the loops though, so this just tunes tiny
         ;; performance difference vs tiny precision differences.
         (epsilon (* 6 SINGLE-FLOAT-NEGATIVE-EPSILON))
         (epsilon*maxdist (* epsilon maxdist)))
    (declare (dynamic-extent tt x v w p s0 s1 s2 s3))
    (declare (type (unsigned-byte 8) dim stuck)
             (type point s0 s1 s2 s3)
             (type single-float maxdist epsilon*maxdist))
    (vsetf ray-normal 0 0 0)
    (loop for i from 0 below GJK-ITERATIONS
          do ;; we test v after updating tt since a final adjustment to
             ;; that improves results slightly. but we need to use the
             ;; values corresponding to v to determine cutoff
             ;; tolerance, so calculate that here
             (setf maxdist (max (if (> dim 0) (vsqrlength s0) 0.0)
                                (if (> dim 1) (vsqrlength s1) 0.0)
                                (if (> dim 2) (vsqrlength s2) 0.0)
                                (if (> dim 3) (vsqrlength s3) 0.0)))
             (setf epsilon*maxdist (* epsilon maxdist))
             (trial:support-function trial:primitive v p)
             (!v- w x p)
             (v<- (point-a s0) p)
             (v<- s0 w)
             (let ((vw (v. v w))
                   (vr (v. v r)))
               (cond ((<= vw 0))
                     ((<= 0 vr)
                      (return NIL))
                     (T
                      (let ((dt (/ vw vr)))
                        (if (= (- tt dt) tt)
                            ;; if we failed to advance, we are
                            ;; probably stuck, so give up after a few
                            ;; tries
                            (incf stuck)
                            (setf stuck 0))
                        (decf tt dt)
                        (v<- ray-normal v)
                        ;; adjust v as well, so we terminate
                        ;; immediately if we reached the
                        ;; simplex. Otherwise we run into problems
                        ;; updating simplex since we don't have enough
                        ;; precision left.
                        (nv+* v r (- dt)))
                      (!v+* x s r tt)
                      (setf last-updated i)
                      ;; update s[0-3] for new X
                      (!v- s0 x (point-a s0))
                      (when (< 0 dim)
                        (!v- s1 x (point-a s1))
                        (when (< 1 dim)
                          (!v- s2 x (point-a s2))
                          (when (< 2 dim)
                            (!v- s3 x (point-a s3))))))))
             (incf dim)
             ;; if point we just added is already in simplex, drop old
             ;; copy since we depend on ordering of vertices and
             ;; degenerate simplexes confuse things anyway. It is
             ;; normal to see duplicates after moving the simplex,
             ;; though duplicates without making any progress tends to
             ;; be a sign we are stuck.
             (cond
               ((and (< 1 dim) (v= (point-a s0) (point-a s1)))
                (decf dim)
                (when (/= i last-updated)
                  (incf stuck))
                (p<- s1 s2)
                (p<- s2 s3))
               ((and (< 2 dim) (v= (point-a s0) (point-a s2)))
                (decf dim)
                (when (/= i last-updated)
                  (incf stuck))
                (p<- s2 s3))
               ((and (< 3 dim) (v= (point-a s0) (point-a s3)))
                (decf dim)
                (when (/= i last-updated)
                  (incf stuck))))
             ;; check if we are done
          while (and (<= epsilon*maxdist (vsqrlength v))
                     (<= stuck 3))
          do ;; update the simplex and find new direction
             (setf dim (signed-volumes dim s0 s1 s2 s3 v epsilon*maxdist))
          while (< dim 4) ;; current point is inside simplex (or on
                          ;; wrong side of simplex), so can't make any
                          ;; more progress.
                          ;; TODO: make sure this returns useful
                          ;; result, and/or see if it can be improved?

          finally (progn
                    (nvunit* ray-normal)
                    (return tt)))))
