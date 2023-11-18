;;; brute-force implementation of ray / convex-mesh intersection test,
;;; and random tester to compare main implementation against it.
(in-package #:org.shirakumo.fraf.trial.gjk)
#++(ql:quickload '(trial random-state))

(defun test-raycast (count &key seed
                             ;; radius within which rays start
                             (start-scale 4.0)
                             ;; scale multiplier of target object (assumes
                             ;; objects are within ±1 to start with)
                             (target-scale 1.0)
                             (normalize-dir t))
  ;;
  (assert (> start-scale (* 2 target-scale)))
  (let* ((seed (or seed (+ (get-internal-real-time)
                           (get-universal-time))))
         (rnd (random-state:make-generator :pcg seed))
         (mesh-cache (make-hash-table)))
    (format t "~%running raycast tests with seed #x~x~%" seed)
    (labels ((get-mesh (type)
               (destructuring-bind (&optional mesh verts)
                   (gethash type mesh-cache)
                 (cond
                   (verts
                    (replace (trial:vertices mesh) verts)
                    mesh)
                   (t
                    (let ((m (trial:coerce-object
                              (ecase type
                                (:box (trial:make-box))
                                (:cylinder (trial:make-cylinder))
                                (:sphere (trial:make-sphere)))
                              'trial:convex-mesh)))
                      (setf (gethash type mesh-cache)
                            (list m (copy-seq (trial:vertices m))))
                      m)))))

             (random-xy<1 ()
               (loop for x = (random-state:random-float rnd -1.0 1.0)
                     for y = (random-state:random-float rnd -1.0 1.0)
                     for x2 = (expt x 2)
                     for y2 = (expt y 2)
                     for z = (+ x2 y2)
                     while (<= 1 z)
                     finally (return (values x y z x2 y2))))
             (random-point-on-sphere (radius)
               (multiple-value-bind (x y z x2 y2) (random-xy<1)
                 (let ((r (sqrt (- 1 x2 y2))))
                   (v* (vec3 (* 2 x r)
                             (* 2 y r)
                             (- 1 (* 2 z)))
                       radius))))
             (random-orientation ()
               (multiple-value-bind (x y z) (random-xy<1)
                 (multiple-value-bind (u v w) (random-xy<1)
                   (let ((s (sqrt (/ (- 1 z) w))))
                     (quat x y (* s u) (* s v))))))
             (random-ray ()
               (let ((q (random-state:random-float rnd 0.0 1.0))
                     (r (random-state:random-float rnd 0.0 1.0))
                     (d 1.0))
                 (cond
                   ;; 25% starting close to unit sphere
                   ((< q 0.25)
                    (v* (random-point-on-sphere 1)
                        (+ (* d target-scale) (* target-scale r))))
                   ;; 25% at 1/2 target-scale
                   ((< q 0.5)
                    (v* (random-point-on-sphere 1)
                        (+ (* d target-scale)
                           (* (- start-scale target-scale) 1/2 r))))
                   ;; rest completely random points between
                   ;; target-scale and start-scale
                   (t
                    (v* (random-point-on-sphere 1)
                        (+ (* d target-scale)
                           (* (- start-scale target-scale)
                              r)))))))
             (random-dir (from)
               (let ((q (random-state:random-float rnd 0.0 1.0)))
                 (cond
                   ;; 25% pointed towards unit sphere at origin
                   ;; (should almost all hit)
                   ((< q 0.25)
                    (v- (random-point-on-sphere target-scale)
                        from))
                   ;; 25% at 2x sized sphere (mostly hit depending on
                   ;; start-scale)
                   ((< q 0.5)
                    (v- (random-point-on-sphere (* 2 target-scale))
                        from))
                   ;; 25% at 4x sized sphere (mostly misses?)
                   ((< q 0.75)
                    (v- (random-point-on-sphere (* 4 target-scale))
                        from))
                   ;; rest completely random
                   (t
                    (random-point-on-sphere 1)))))
             (random-obj (type orientation)
               (let* ((mat (qmat orientation))
                      (mesh (get-mesh type))
                      (obj (make-instance 'trial:rigidbody
                                          :physics-primitives (vector mesh))))
                 (org.shirakumo.fraf.manifolds:transform-mesh
                  (trial:vertices mesh) mat)
                 obj))
             (save-case (start dir type orientation &rest keys)
               (declare (ignore start dir type orientation keys))
               ;; todo: do something with these
               ))
      (loop with hits = 0
            with ref-hits = 0
            with miss = 0
            with steps = 0
            with steps-hist = (make-hash-table)
            with stuck = 0
            with ins = 0
            with ins-no-hit = 0
            with distances = (make-array count :fill-pointer 0
                                               :initial-element 0.0)
            repeat count
            for i from 0
            for start = (random-ray)
            for dir = (if normalize-dir
                          (nvunit (random-dir start))
                          (random-dir start))
            for ray = (trial:ray start dir)
            for orientation = (random-orientation)
            for otype = (ecase (random-state:random-int rnd 0 2)
                          (0 :box)
                          (1 :cylinder)
                          (2 :sphere))
            for obj = (random-obj otype orientation)
            for (ref nil in) = (multiple-value-list
                                (ref start dir (aref (trial:physics-primitives obj) 0)))
            for (.hit err) = (multiple-value-list
                              (ignore-errors
                               (trial:detect-hit ray obj)))
            for hit = (when .hit
                        (trial:hit-location .hit))
            for dist = (when (and ref hit)
                         (vdistance hit ref))
            when (and (not err)
                      (or (alexandria:xor hit ref)
                          (and dist (> dist 0.001) (not in))))
              do (let ((or ref)
                       (od dist)
                       (oin in)
                       n)
                   (setf (values ref n in)
                         (ref/rat start dir
                                  (aref (trial:physics-primitives obj) 0)))
                   (setf dist (when (and hit ref) (vdistance hit ref)))
                   (when (when (or (and or ref (< 0.0001 (vdistance or ref)))
                                   (alexandria:xor or ref)))
                     (format t "~&try rat/ref: moved ~s~%    ~s~% -> ~s~% in ~s -> ~s~%"
                             (when (and or ref)
                               (vdistance or ref))
                             or ref oin in)
                     (format t "  dist ~s -> ~s~%" od dist)))
            when err
              do (format t "~&error: #x~x @ ~s~%  ~a~%hit ~s~%  ref ~s~%"
                         seed i err hit ref)
                 (format t "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                         start dir otype orientation)
                 (save-case start dir otype orientation
                            :hit hit :ref ref :in in)
            else when (alexandria:xor hit ref)
                   do (format t "~&mismatch: #x~x @ ~s~%  hit ~s~%  ref ~s~%"
                              seed i hit ref)
                      (format t "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                              start dir otype orientation)
                      (save-case start dir otype orientation
                                 :hit hit :ref ref :in in)
            when (and dist (> dist 0.01) (not in))
              do (format t "~&dist ~s > 0.01? #x~x @ ~s~%  hit ~s~%  ref ~s~%"
                         dist seed i hit ref)
                 (format t "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                         start dir otype orientation)
                 (save-case start dir otype orientation
                            :dist dist :hit hit :ref ref :in in)
            when (zerop (mod i 1000))
              do (format t "~s~%" i)
            else do (when (zerop (mod i 10)) (format t "."))
            do (if hit (incf hits) (incf miss))
               (when in (incf ins) (unless hit (incf ins-no-hit)))
               (when ref (incf ref-hits))
               (when (and dist (not in))
                 (vector-push-extend dist distances))
            #++
             (let ((s (gethash :steps *debug-state* 0)))
               (incf steps s)
               (incf (gethash s steps-hist 0))
               (when (< 60 s)
                 (incf stuck)
                 (format t "~&step=~s: #x~x @ ~s~%"
                         (gethash :steps *debug-state* 0) seed i)))
            finally
               (format t "~&~s hits (~s ref) / ~s miss @ :seed #x~x~%"
                       hits ref-hits miss seed)
               (when ins
                 (format t "~s start in object (~s not detected)~%"
                         ins ins-no-hit))
               (unless (zerop steps)
                 (format t "~s steps total (avg ~s) | ~s stuck~%"
                         steps (float (/ steps count)) stuck))
               (format t "max dist ~s~%" (reduce 'max distances))
               (format t "mean ~s, median ~s, dev ~s, variance ~s~%"
                       (alexandria:mean distances)
                       (alexandria:median distances)
                       (alexandria:standard-deviation distances)
                       (alexandria:variance distances))
               (unless (zerop (hash-table-count steps-hist))
                 (format t "steps:~%~{  ~s ~s~%~}"
                         (alexandria:alist-plist
                          (sort (alexandria:hash-table-alist steps-hist)
                                '< :key 'car))))))))

;; very fast check
#++
(time (test-raycast 100 :start-scale 128 :seed #x12345678))
"
33 hits (33 ref) / 67 miss @ :seed #x12345678
1 start in object (0 not detected)
max dist 7.443701e-5
mean 9.266094e-6, median 1.1832974e-6, dev 1.7844535e-5, variance 3.1842745e-10
steps:
Evaluation took:
  0.028 seconds of real time
  0.031250 seconds of total run time (0.031250 user, 0.000000 system)
  110.71% CPU
  114,234,991 processor cycles
  34,639,680 bytes consed"
;; fairly fast check
#++
(time (test-raycast 10000 :start-scale 128 :seed #x12345))
"
3195 hits (3194 ref) / 6805 miss @ :seed #x12345
191 start in object (0 not detected)
23130 steps total (avg 2.313) | 0 stuck
max dist 0.004599267
mean 1.5354657e-5, median 2.1324806e-6, dev 1.1566713e-4, variance 1.3378885e-8
steps:
  0 6805
  3 6
  4 1129
  5 385
  6 153
  7 157
  8 199
  9 181
  10 294
  11 296
  12 192
  13 78
  14 44
  15 26
  16 30
  17 21
  18 1
  19 2
  20 1
Evaluation took:
  4.694 seconds of real time
  4.046875 seconds of total run time (3.609375 user, 0.437500 system)
  [ Real times consist of 0.024 seconds GC time, and 4.670 seconds non-GC time. ]
  86.22% CPU
  18,816,221,175 processor cycles
  3,937,788,192 bytes consed"

;; longer test
#++
(time (test-raycast 100000 :start-scale 128 :seed #x123456))
"32346 hits (32339 ref) / 67654 miss @ :seed #x123456
2061 start in object (0 not detected)
234762 steps total (avg 2.34762) | 0 stuck
max dist 0.00759323
mean 1.4840219e-5, median 2.1324806e-6, dev 1.0068996e-4, variance 1.0138468e-8
steps:
  0 67654
  3 67
  4 10994
  5 4137
  6 1346
  7 1701
  8 2387
  9 1776
  10 3132
  11 3013
  12 1769
  13 880
  14 391
  15 284
  16 238
  17 118
  18 56
  19 21
  20 20
  21 8
  22 4
  23 2
  24 1
  25 1
Evaluation took:
  43.330 seconds of real time
  35.484375 seconds of total run time (30.812500 user, 4.671875 system)
  [ Real times consist of 0.195 seconds GC time, and 43.135 seconds non-GC time. ]
  [ Run times consist of 0.171 seconds GC time, and 35.314 seconds non-GC time. ]
  81.89% CPU
  173,669,026,003 processor cycles
  37,895,086,720 bytes consed"

;; consing test
#++
(let* ((ray (trial:ray (vec3 1 2 3)
                       (nvunit (vec3 -2 -7 -9))))
       (mesh (trial:coerce-object (trial:make-sphere) 'trial:convex-mesh))
       (obj (make-instance 'trial:rigidbody :physics-primitives (vector mesh))))
  (count-if 'identity
            (time (loop repeat 1000 collect (trial:detect-hit ray obj)))))
"Evaluation took:
  0.197 seconds of real time
  0.187500 seconds of total run time (0.187500 user, 0.000000 system)
  95.43% CPU
  790,091,328 processor cycles
  425,936 bytes consed"
;; if ray-primitive test just returns 0, it still conses about 320k
" 1,422,150 processor cycles
  327,360 bytes consed"


(defun map-convex-mesh-faces (|(f v1 v2 v3)| primitive)
  (let ((verts (trial:convex-mesh-vertices primitive))
        (faces (trial:convex-mesh-faces primitive))
        (v1 (vec3))
        (v2 (vec3))
        (v3 (vec3)))
    (declare (dynamic-extent v1 v2 v3))
    (loop for f from 0 below (length faces) by 3
          do (flet ((vert (v i)
                      (let ((i (* 3 i)))
                        (vsetf v
                               (aref verts (+ i 0))
                               (aref verts (+ i 1))
                               (aref verts (+ i 2))))
                      v))
               (funcall |(f v1 v2 v3)|
                        (vert v1 (aref faces (+ f 0)))
                        (vert v2 (aref faces (+ f 1)))
                        (vert v3 (aref faces (+ f 2))))))))

(defun ref (ray-start ray-dir o)
  (let* ((d most-positive-single-float) ;; closest tₙ seen so far
         (p (vec3))
         (n (vec3))
         (found nil)
         (found-behind 0)
         (p- (vec3))
         (n- (vec3))
         (d- most-positive-single-float)
         (ϵ 0.00001)
         (ϵ² (expt ϵ 2)))
    (map-convex-mesh-faces
     (lambda (a b c)
       (let* ((ab (v- b a))
              (ac (v- c a))
              (ab×ac (vc ab ac))
              (-rd (v- ray-dir))
              (det (v. -rd ab×ac))
              (as (v- ray-start a))
              (t₁ (v. ab×ac as))
              ;;(t₂ (v. (vc ac -rd) ap))
              ;;(t₃ (v. (vc -rd ab) ap))
              )
         (cond
           ;; ray is (almost) parallel to or on plane of triangle, test edges
           ((<= (abs det) ϵ)
            (when (<= (abs t₁) ϵ) ;; distance from ray-start to plane
              (flet ((edge (a ab)
                       (let* ((as (v- ray-dir ab))
                              (n₁ (vc ray-dir ab))
                              (n₂ (vc ab n₁))
                              ;; line 1 = ray-start + t₁*ray-dir/d₁
                              (t₁ (v. as n₂))
                              (d₁ (v. ray-dir n₂))
                              ;; line 2 = a + t₂*ab/d₂
                              (t₂ (- (v. as n₁)))
                              (d₂ (v. ab n₁)))
                         (when (and
                                ;; lines are not parallel
                                (> (abs d₁) ϵ)
                                (> (abs d₂) ϵ)
                                ;; closest point is in ray
                                (> t₁ (- ϵ))
                                ;; and in edge
                                (> (1+ ϵ) t₂ (- ϵ)))
                           (let* ((t₁/d₁ (/ t₁ d₁))
                                  (p₁ (v+* ray-start ray-dir t₁/d₁))
                                  (p₂ (v+* a ab (/ t₂ d₂))))
                             (when (and
                                    ;;points are (nearly) same point
                                    (< (vsqrdistance p₁ p₂) ϵ²)
                                    ;; and closest point so far
                                    (< t₁/d₁ d))
                               (setf found t
                                     d t₁/d₁)
                               (v<- p p₁)
                               (!vc n (!vc n ab ray-dir) ab)))))))
                (edge a ab)
                (edge a ac)
                (edge b (v- c b)))))
           ;; ray intersects plane at ray-start + t₁*ray-dir
           ((<= 0 (setf t₁ (/ t₁ det)))
            (let* ((p₀ (v+* ray-start ray-dir t₁))
                   (ap (v- p₀ a))
                   (l (vsqrlength ab×ac))
                   (λ₂ (/ (v. (vc ap ac) ab×ac) l))
                   (λ₃ (/ (v. (vc ab ap) ab×ac) l)))
              ;; test if point is in triangle
              (when (and (< (- ϵ) λ₂ (1+ ϵ))
                         (< (- ϵ) λ₃ (1+ ϵ))
                         (< (+ λ₂ λ₃) (1+ ϵ)))
                (when (< t₁ d)
                  (setf found t
                        d t₁)
                  (v<- p p₀)
                  (v<- n ab×ac)))))
           ;; intersects plane behind ray. If start is inside object,
           ;; we want to return hit behind start, but if entire object
           ;; is behind ray, we return NIL
           (t
            ;; t₁ already divided by det in previous test
            (let* ((p₀ (v+* ray-start ray-dir t₁))
                   (ap (v- p₀ a))
                   (l (vsqrlength ab×ac))
                   (λ₂ (/ (v. (vc ap ac) ab×ac) l))
                   (λ₃ (/ (v. (vc ab ap) ab×ac) l)))
              ;; test if point is in triangle
              (when (and (< (- ϵ) λ₂ (1+ ϵ))
                         (< (- ϵ) λ₃ (1+ ϵ))
                         (< (+ λ₂ λ₃) (1+ ϵ)))
                ;; count # of hits behind ray
                (incf found-behind)
                ;; and remember best hit behind ray
                (when (< t₁ d-)
                  (setf d- t₁)
                  (v<- p- p₀)
                  (v<- n- ab×ac))))))))
     o)
    (cond
      ((and found (not (zerop found-behind)))
       ;;(assert (= found-behind 1)) ;; this happens sometimes :/
       (values p- (nvunit n) t))
      (found
       (values p (nvunit n) nil)))))


(defun rv (a)
  (vec3 (float (aref a 0))
        (float (aref a 1))
        (float (aref a 2))))

(defun r. (a b)
  (reduce '+ (map 'vector '* a b)))
(defun r- (a &optional b)
  (if b
      (map 'vector '- a b)
      (map 'vector '- a)))
(defun r+* (a b c)
  (map 'vector (lambda (a b) (+ a (* b c))) a b))
(defun rc (a b)
  (vector (- (* (aref a 1) (aref b 2))
             (* (aref a 2) (aref b 1)))
          (- (* (aref a 2) (aref b 0))
             (* (aref a 0) (aref b 2)))
          (- (* (aref a 0) (aref b 1))
             (* (aref a 1) (aref b 0)))))
(defun rsqrdistance (a b)
  (let ((d (v- a b)))
    (v. d d)))

(defun rat-line-plane (rs rd pn p0)
  ;; line p=rs+t*rd, plane (p-p₀)*n=0
  (flet ((rat (x) (rational x)))
    (let* ((l0 (map 'vector #'rat rs))
           (l (map 'vector #'rat rd))
           (n (map 'vector #'rat pn))
           ;;(dl (print (reduce '+ (map 'vector (alexandria:rcurry 'expt 2) n))))
           (p0 (map 'vector #'rat p0))
           (l⋅n (r. l n))
           (p0-l0 (map 'vector '- p0 l0))
           (p0-l0⋅n (r. p0-l0 n))
           (d (unless (zerop l⋅n)
                (/ p0-l0⋅n l⋅n)))
           (r (when d
                (map 'vector '+ l0 (map 'vector (alexandria:curry '* d) l)))))
      (values (map 'vector 'float r) r))))


(defun rat-point-line (rs rd p0)
  ;; line p=rs+t*rd, point p0
  (flet ((rat (x) (rational x)))
    (let* ((b (map 'vector #'rat rs))
           (m (map 'vector #'rat rd))
           (p (map 'vector #'rat p0))
           (p-b (map 'vector '- p b))
           (m⋅p-b (reduce '+ (map 'vector '* m p-b)))
           (m⋅m (reduce '+ (map 'vector '* m m)))
           (t0 (unless (zerop m⋅m)
                 (/ m⋅p-b m⋅m)))
           (p1 (when t0
                 (map 'vector '+ b (map 'vector (alexandria:curry '* t0) m))))
           (d² (when p1
                 (reduce '+ (map 'vector (alexandria:rcurry 'expt 2)
                                 (map 'vector '- p1 p))))))
      (values (sqrt d²) (float d²) d²
              (map 'vector 'float p1)))))

(defun ref/rat (.ray-start .ray-dir o)
  (flet ((rat (a) (map 'vector 'rational (varr a))))
    (let* ((d most-positive-single-float) ;; closest tₙ seen so far
           (ray-start (rat .ray-start))
           (ray-dir (rat .ray-dir))
           (p (vec3))
           (n (vec3))
           (found nil)
           (found-behind 0)
           (p- (vec3))
           (n- (vec3))
           (d- most-positive-single-float))
      (map-convex-mesh-faces
       (lambda (.a .b .c)
         (let* ((a (rat .a))
                (b (rat .b))
                (c (rat .c))
                (ab (r- b a))
                (ac (r- c a))
                (ab×ac (rc ab ac))
                (-rd (r- ray-dir))
                (det (r. -rd ab×ac))
                (as (r- ray-start a))
                (t₁ (r. ab×ac as))
                ;;(t₂ (v. (vc ac -rd) ap))
                ;;(t₃ (v. (vc -rd ab) ap))
                )
           (cond
             ;; ray is (almost) parallel to or on plane of triangle, test edges
             ((= det 0)
              (when (= t₁ 0) ;; distance from ray-start to plane
                (flet ((edge (a ab)
                         (let* ((as (r- ray-dir ab))
                                (n₁ (rc ray-dir ab))
                                (n₂ (rc ab n₁))
                                ;; line 1 = ray-start + t₁*ray-dir/d₁
                                (t₁ (r. as n₂))
                                (d₁ (r. ray-dir n₂))
                                ;; line 2 = a + t₂*ab/d₂
                                (t₂ (- (r. as n₁)))
                                (d₂ (r. ab n₁)))
                           (when (and
                                  ;; lines are not parallel
                                  (/= d₁ 0)
                                  (/= d₂ 0)
                                  ;; closest point is in ray
                                  (>= t₁ 0)
                                  ;; and in edge
                                  (>= 1 t₂ 0))
                             (let* ((t₁/d₁ (/ t₁ d₁))
                                    (p₁ (r+* ray-start ray-dir t₁/d₁))
                                    (p₂ (r+* a ab (/ t₂ d₂))))
                               (when (and
                                      ;;points are (nearly) same point
                                      (= (rsqrdistance p₁ p₂) 0)
                                      ;; and closest point so far
                                      (< t₁/d₁ d))
                                 (setf found t
                                       d t₁/d₁)
                                 (v<- p (rv p₁))
                                 (setf n (rc (rc ab ray-dir) ab))))))))
                  (edge a ab)
                  (edge a ac)
                  (edge b (r- c b)))))
             ;; ray intersects plane at ray-start + t₁*ray-dir
             ((<= 0 (setf t₁ (/ t₁ det)))
              (let* ((p₀ (r+* ray-start ray-dir t₁))
                     (ap (r- p₀ a))
                     (l (r. ab×ac ab×ac))
                     (λ₂ (/ (r. (rc ap ac) ab×ac) l))
                     (λ₃ (/ (r. (rc ab ap) ab×ac) l)))
                ;; test if point is in triangle
                (when (and (<= 0 λ₂ 1)
                           (<= 0 λ₃ 1)
                           (<= (+ λ₂ λ₃) 1))
                  (when (<= t₁ d)
                    (setf found t
                          d t₁)
                    (v<- p (rv p₀))
                    (v<- n (rv ab×ac))))))
             ;; intersects plane behind ray. If start is inside
             ;; object, we want to return hit behind start, but if
             ;; entire object is behind ray, we return NIL
             (t
              ;; t₁ already divided by det in previous test
              (let* ((p₀ (r+* ray-start ray-dir t₁))
                     (ap (r- p₀ a))
                     (l (r. ab×ac ab×ac))
                     (λ₂ (/ (r. (rc ap ac) ab×ac) l))
                     (λ₃ (/ (r. (rc ab ap) ab×ac) l)))
                ;; test if point is in triangle
                (when (and (<= 0 λ₂ 1)
                           (<= 0 λ₃ 1)
                           (<= (+ λ₂ λ₃) 1))
                  ;; count # of hits behind ray
                  (incf found-behind)
                  ;; and remember best hit behind ray
                  (when (< t₁ d-)
                    (setf d- t₁)
                    (v<- p- (rv p₀))
                    (v<- n- (rv ab×ac)))))))))
       o)
      (cond
        ((and found (not (zerop found-behind)))
         (assert (= found-behind 1))
         (values p- (nvunit n) t))
        (found
         (values p (nvunit n) nil))))))
