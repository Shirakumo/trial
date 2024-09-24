;;; brute-force implementation of ray / convex-mesh intersection test,
;;; and random tester to compare main implementation against it.
(in-package #:org.shirakumo.fraf.trial.gjk)
#++(ql:quickload '(trial random-state))

(defvar *stats* (make-hash-table))
(defvar *err-stats* (make-hash-table))
(defun test-raycast (count &key seed
                             ;; radius within which rays start
                             (start-scale 4.0)
                             ;; scale multiplier of target object (assumes
                             ;; objects are within ±1 to start with)
                             (target-scale 1.0)
                             (normalize-dir T)
                             ;; pills are very slow :/
                             (test-pills nil))
  ;;
  (assert (> start-scale (* 2 target-scale)))
  (let* ((seed (or seed (+ (get-internal-real-time)
                           (get-universal-time))))
         (rnd (random-state:make-generator :pcg seed))
         (mesh-cache (make-hash-table)))
    (format T "~%running raycast tests with seed #x~x~%" seed)
    (labels ((get-mesh (type)
               (destructuring-bind (&optional mesh verts)
                   (gethash type mesh-cache)
                 (cond
                   (verts
                    (replace (trial::vertices mesh) verts)
                    mesh)
                   (T
                    (let ((m (trial:coerce-object
                              (ecase type
                                (:box (trial:make-box))
                                (:cylinder (trial:make-cylinder))
                                (:cone (trial:make-cone))
                                (:sphere (trial:make-sphere))
                                (:pill (trial:make-pill))
                                (:ellipsoid (trial:make-ellipsoid)))
                              'trial:convex-mesh)))
                      (setf (gethash type mesh-cache)
                            (list m (copy-seq (trial::vertices m))))
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
                   (T
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
                   (T
                    (random-point-on-sphere 1)))))
             (random-obj (type orientation)
               (let* ((mat (m* (qmat orientation)
                               (mblock (mscaling (vec3 target-scale))
                                       0 0 3 3)))
                      (mesh (get-mesh type))
                      (obj (make-instance 'trial:rigidbody
                                          :physics-primitives (vector mesh))))
                 (org.shirakumo.fraf.manifolds:transform-mesh
                  (trial::vertices mesh) mat)
                 ;; assign to update bounds etc
                 (setf (trial::vertices mesh) (trial::vertices mesh))
                 obj))
             (save-case (start dir type orientation &rest keys)
               ;; todo: do something with these
               (declare (ignorable start dir type orientation keys))
               (incf (gethash type *err-stats* 0))))
      (loop with hits = 0
            with ref-hits = 0
            with miss = 0
            with steps = 0
            with steps-hist = (make-hash-table)
            with stuck = 0
            with error-count = 0
            with mismatch- = 0
            with mismatch+ = 0
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
            for otype = (ecase (random-state:random-int
                                rnd 0
                                (if test-pills 3 2))
                          (0 :box)
                          (1 :cylinder)
                          (2 :sphere)
                          (3 :pill)
                          ;; todo: random shape ellipsoids?
                          ;; or just add random non-uniform scaling to all
                          ;; since they are converted to meshes anyway?
                          #++(4 :ellipsoid)
                          ;; todo: add cones? other shapes?
                          #++(5 :cone))
            for obj = (random-obj otype orientation)
            for (ref NIL in) = (multiple-value-list
                                (ref start dir (aref (trial:physics-primitives obj) 0)))
            for (.hit err) = (multiple-value-list
                              (ignore-errors
                               (trial:detect-hit ray obj)))
            for hit = (when .hit
                        (trial:hit-location .hit))
            for dist = (when (and ref hit)
                         (vdistance hit ref))
            do (incf (gethash otype *stats* 0))
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
                     (format T "~&try rat/ref: moved ~s~%    ~s~% -> ~s~% in ~s -> ~s~%"
                             (when (and or ref)
                               (vdistance or ref))
                             or ref oin in)
                     (format T "  dist ~s -> ~s~%" od dist)))
            when err
              do (format T "~&error: #x~x @ ~s~%  ~a~%hit ~s~%  ref ~s~%"
                         seed i err hit ref)
                 (format T "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                         start dir otype orientation)
                 (incf error-count)
                 (save-case start dir otype orientation
                            :hit hit :ref ref :in in)
            else when (alexandria:xor hit ref)
                   do (format T "~&mismatch: #x~x @ ~s~%  hit ~s~%  ref ~s~%"
                              seed i hit ref)
                      (format T "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                              start dir otype orientation)
                      (if hit
                          (incf mismatch+)
                          (incf mismatch-))
                      (save-case start dir otype orientation
                                 :hit hit :ref ref :in in)
            when (and dist (> dist (* 0.01 target-scale)) (not in))
              do (format T "~&dist ~s > ~a? #x~x @ ~s~%  hit ~s~%  ref ~s~%"
                         dist (* 0.01 target-scale) seed i hit ref)
                 (format T "--start=~s~%  dir=~s~%  obj=~s @ ~s~%"
                         start dir otype orientation)
                 (flet ((dv- (a b)
                          (map '(simple-array double-float (3))
                               (lambda (a b)
                                 (- (coerce a 'double-float)
                                    (coerce b 'double-float)))
                               (varr a) (varr b)))
                        (|| (a)
                          (sqrt (loop for i across a
                                      sum (expt i 2))))
                        (dot (a b)
                          (reduce '+ (map 'vector '* a b))))
                   (let* ((d1 (dv- hit start))
                          (d2 (dv- ref start))
                          (l (* (|| d1) (|| d2)))
                          (cos (unless (zerop l) (/ (dot d1 d2) l)))
                          (a (when cos (acos (alexandria:clamp cos -1 1)))))
                     (format T "  ~aangle = ~10,8f° (~10,9f)~%"
                             (if (and a (> a 0.00002d0)) ;; 0.001°
                                 "!!!!"
                                 "")
                             (when a (* a (/ 180 PI))) a)))
                 (save-case start dir otype orientation
                            :dist dist :hit hit :ref ref :in in)
            when (zerop (mod i 1000))
              do (format T "~s~%" i)
            else do (when (zerop (mod i 10)) (format T "."))
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
                 (format T "~&step=~s: #x~x @ ~s~%"
                         (gethash :steps *debug-state* 0) seed i)))
            finally
               (format T "~&~s hits (~s ref +~s -~s) / ~s miss @ :seed #x~x~%"
                       hits ref-hits mismatch+ mismatch- miss seed)
               (when error-count
                 (format t "~s errors~%" error-count))
               (when ins
                 (format T "~s start in object (~s not detected)~%"
                         ins ins-no-hit))
               (unless (zerop steps)
                 (format T "~s steps total (avg ~s) | ~s stuck~%"
                         steps (float (/ steps count)) stuck))
               (unless (alexandria:emptyp distances)
                 (format T "max dist ~s~%" (reduce 'max distances))
                 (format T "mean ~s, median ~s, dev ~s, variance ~s~%"
                         (alexandria:mean distances)
                         (alexandria:median distances)
                         (alexandria:standard-deviation distances)
                         (alexandria:variance distances)))
               (unless (zerop (hash-table-count steps-hist))
                 (format T "steps:~%~{  ~s ~s~%~}"
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

;; large dynamic range test
#++
(time (test-raycast 30000 :start-scale 1024 :target-scale 510 :seed #x123456))
"
17701 hits (17690 ref +11 -0) / 12299 miss @ :seed #x123456
0 errors
4391 start in object (0 not detected)
max dist 2.4737363
mean 0.0064291866, median 2.8300838e-4, dev 0.04605692, variance 0.0021212397
Evaluation took:
  99.593 seconds of real time
  99.718750 seconds of total run time (93.296875 user, 6.421875 system)
  [ Real times consist of 0.237 seconds GC time, and 99.356 seconds non-GC time. ]
  [ Run times consist of 0.250 seconds GC time, and 99.469 seconds non-GC time. ]
  100.13% CPU
  399,170,634,802 processor cycles
  49,713,068,672 bytes consed
  "

;; large dynamic range test with pills
#++
(time (test-raycast 10000 :start-scale 1024 :target-scale 510 :seed #x123456
                    :test-pills t))
"6113 hits (6109 ref +4 -0) / 3887 miss @ :seed #x123456
0 errors
1847 start in object (0 not detected)
max dist 99.870094
mean 0.54339653, median 4.792437e-4, dev 2.5036151, variance 6.268089
Evaluation took:
  191.441 seconds of real time
  190.562500 seconds of total run time (179.609375 user, 10.953125 system)
  [ Real times consist of 0.394 seconds GC time, and 191.047 seconds non-GC time. ]
  [ Run times consist of 0.406 seconds GC time, and 190.157 seconds non-GC time. ]
  99.54% CPU
  767,297,452,271 processor cycles
  88,014,335,168 bytes consed
  "

;; very large dynamic range test
#++
(time (test-raycast 10000 :start-scale 4096 :target-scale 2040 :seed #x54321))
"5930 hits (5927 ref +3 -0) / 4070 miss @ :seed #x54321
0 errors
1456 start in object (0 not detected)
max dist 38.043087
mean 0.03409025, median 0.0010589204, dev 0.5979002, variance 0.3574847
Evaluation took:
  82.366 seconds of real time
  82.234375 seconds of total run time (77.625000 user, 4.609375 system)
  [ Real times consist of 0.182 seconds GC time, and 82.184 seconds non-GC time. ]
  [ Run times consist of 0.187 seconds GC time, and 82.048 seconds non-GC time. ]
  99.84% CPU
  330,123,127,850 processor cycles
  38,206,471,904 bytes consed"


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
  (let* ((d MOST-POSITIVE-SINGLE-FLOAT) ;; closest tₙ seen so far
         (p (vec3))
         (n (vec3))
         (found NIL)
         (found-behind 0)
         (p- (vec3))
         (n- (vec3))
         (d- MOST-POSITIVE-SINGLE-FLOAT)
         (epsilon 0.00001)
         (epsilon^2 (expt epsilon 2)))
    (map-convex-mesh-faces
     (lambda (a b c)
       (let* ((ab (v- b a))
              (ac (v- c a))
              (ab*ac (vc ab ac))
              (-rd (v- ray-dir))
              (det (v. -rd ab*ac))
              (as (v- ray-start a))
              (t1 (v. ab*ac as))
              ;;(t2 (v. (vc ac -rd) ap))
              ;;(t3 (v. (vc -rd ab) ap))
              )
         (cond
           ;; ray is (almost) parallel to or on plane of triangle, test edges
           ((<= (abs det) epsilon)
            (when (<= (abs t1) epsilon) ;; distance from ray-start to plane
              (flet ((edge (a ab)
                       (let* ((as (v- ray-dir ab))
                              (n1 (vc ray-dir ab))
                              (n2 (vc ab n1))
                              ;; line 1 = ray-start + t1*ray-dir/d1
                              (t1 (v. as n2))
                              (d1 (v. ray-dir n2))
                              ;; line 2 = a + t2*ab/d2
                              (t2 (- (v. as n1)))
                              (d2 (v. ab n1)))
                         (when (and
                                ;; lines are not parallel
                                (> (abs d1) epsilon)
                                (> (abs d2) epsilon)
                                ;; closest point is in ray
                                (> t1 (- epsilon))
                                ;; and in edge
                                (> (1+ epsilon) t2 (- epsilon)))
                           (let* ((t1/d1 (/ t1 d1))
                                  (p1 (v+* ray-start ray-dir t1/d1))
                                  (p2 (v+* a ab (/ t2 d2))))
                             (when (and
                                    ;;points are (nearly) same point
                                    (< (vsqrdistance p1 p2) epsilon^2)
                                    ;; and closest point so far
                                    (< t1/d1 d))
                               (setf found T
                                     d t1/d1)
                               (v<- p p1)
                               (!vc n (!vc n ab ray-dir) ab)))))))
                (edge a ab)
                (edge a ac)
                (edge b (v- c b)))))
           ;; ray intersects plane at ray-start + t1*ray-dir
           ((<= 0 (setf t1 (/ t1 det)))
            (let* ((p0 (v+* ray-start ray-dir t1))
                   (ap (v- p0 a))
                   (l (vsqrlength ab*ac))
                   (t2 (/ (v. (vc ap ac) ab*ac) l))
                   (t3 (/ (v. (vc ab ap) ab*ac) l)))
              ;; test if point is in triangle
              (when (and (< (- epsilon) t2 (1+ epsilon))
                         (< (- epsilon) t3 (1+ epsilon))
                         (< (+ t2 t3) (1+ epsilon)))
                (when (< t1 d)
                  (setf found T
                        d t1)
                  (v<- p p0)
                  (v<- n ab*ac)))))
           ;; intersects plane behind ray. If start is inside object,
           ;; we want to return hit behind start, but if entire object
           ;; is behind ray, we return NIL
           (T
            ;; t1 already divided by det in previous test
            (let* ((p0 (v+* ray-start ray-dir t1))
                   (ap (v- p0 a))
                   (l (vsqrlength ab*ac))
                   (t2 (/ (v. (vc ap ac) ab*ac) l))
                   (t3 (/ (v. (vc ab ap) ab*ac) l)))
              ;; test if point is in triangle
              (when (and (< (- epsilon) t2 (1+ epsilon))
                         (< (- epsilon) t3 (1+ epsilon))
                         (< (+ t2 t3) (1+ epsilon)))
                ;; count # of hits behind ray
                (incf found-behind)
                ;; and remember best hit behind ray
                (when (< t1 d-)
                  (setf d- t1)
                  (v<- p- p0)
                  (v<- n- ab*ac))))))))
     o)
    (cond
      ((and found (not (zerop found-behind)))
       ;;(assert (= found-behind 1)) ;; this happens sometimes :/
       (values p- (nvunit n) T))
      (found
       (values p (nvunit n) NIL)))))


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
  ;; line p=rs+t*rd, plane (p-p0)*n=0
  (flet ((rat (x) (rational x)))
    (let* ((l0 (map 'vector #'rat rs))
           (l (map 'vector #'rat rd))
           (n (map 'vector #'rat pn))
           ;;(dl (print (reduce '+ (map 'vector (alexandria:rcurry 'expt 2) n))))
           (p0 (map 'vector #'rat p0))
           (l.n (r. l n))
           (p0-l0 (map 'vector '- p0 l0))
           (p0-l0.n (r. p0-l0 n))
           (d (unless (zerop l.n)
                (/ p0-l0.n l.n)))
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
           (m.p-b (reduce '+ (map 'vector '* m p-b)))
           (m.m (reduce '+ (map 'vector '* m m)))
           (t0 (unless (zerop m.m)
                 (/ m.p-b m.m)))
           (p1 (when t0
                 (map 'vector '+ b (map 'vector (alexandria:curry '* t0) m))))
           (d^2 (when p1
                 (reduce '+ (map 'vector (alexandria:rcurry 'expt 2)
                                 (map 'vector '- p1 p))))))
      (values (sqrt d^2) (float d^2) d^2
              (map 'vector 'float p1)))))

(defun ref/rat (.ray-start .ray-dir o)
  (flet ((rat (a) (map 'vector 'rational (varr a))))
    (let* ((d MOST-POSITIVE-SINGLE-FLOAT) ;; closest tₙ seen so far
           (ray-start (rat .ray-start))
           (ray-dir (rat .ray-dir))
           (p (vec3))
           (n (vec3))
           (found NIL)
           (found-behind 0)
           (p- (vec3))
           (n- (vec3))
           (d- MOST-POSITIVE-SINGLE-FLOAT))
      (map-convex-mesh-faces
       (lambda (.a .b .c)
         (let* ((a (rat .a))
                (b (rat .b))
                (c (rat .c))
                (ab (r- b a))
                (ac (r- c a))
                (ab*ac (rc ab ac))
                (-rd (r- ray-dir))
                (det (r. -rd ab*ac))
                (as (r- ray-start a))
                (t1 (r. ab*ac as))
                ;;(t2 (v. (vc ac -rd) ap))
                ;;(t3 (v. (vc -rd ab) ap))
                )
           (cond
             ;; ray is (almost) parallel to or on plane of triangle, test edges
             ((= det 0)
              (when (= t1 0) ;; distance from ray-start to plane
                (flet ((edge (a ab)
                         (let* ((as (r- ray-dir ab))
                                (n1 (rc ray-dir ab))
                                (n2 (rc ab n1))
                                ;; line 1 = ray-start + t1*ray-dir/d1
                                (t1 (r. as n2))
                                (d1 (r. ray-dir n2))
                                ;; line 2 = a + t2*ab/d2
                                (t2 (- (r. as n1)))
                                (d2 (r. ab n1)))
                           (when (and
                                  ;; lines are not parallel
                                  (/= d1 0)
                                  (/= d2 0)
                                  ;; closest point is in ray
                                  (>= t1 0)
                                  ;; and in edge
                                  (>= 1 t2 0))
                             (let* ((t1/d1 (/ t1 d1))
                                    (p1 (r+* ray-start ray-dir t1/d1))
                                    (p2 (r+* a ab (/ t2 d2))))
                               (when (and
                                      ;;points are (nearly) same point
                                      (= (rsqrdistance p1 p2) 0)
                                      ;; and closest point so far
                                      (< t1/d1 d))
                                 (setf found T
                                       d t1/d1)
                                 (v<- p (rv p1))
                                 (setf n (rc (rc ab ray-dir) ab))))))))
                  (edge a ab)
                  (edge a ac)
                  (edge b (r- c b)))))
             ;; ray intersects plane at ray-start + t1*ray-dir
             ((<= 0 (setf t1 (/ t1 det)))
              (let* ((p0 (r+* ray-start ray-dir t1))
                     (ap (r- p0 a))
                     (l (r. ab*ac ab*ac))
                     (t2 (/ (r. (rc ap ac) ab*ac) l))
                     (t3 (/ (r. (rc ab ap) ab*ac) l)))
                ;; test if point is in triangle
                (when (and (<= 0 t2 1)
                           (<= 0 t3 1)
                           (<= (+ t2 t3) 1))
                  (when (<= t1 d)
                    (setf found T
                          d t1)
                    (v<- p (rv p0))
                    (v<- n (rv ab*ac))))))
             ;; intersects plane behind ray. If start is inside
             ;; object, we want to return hit behind start, but if
             ;; entire object is behind ray, we return NIL
             (T
              ;; t1 already divided by det in previous test
              (let* ((p0 (r+* ray-start ray-dir t1))
                     (ap (r- p0 a))
                     (l (r. ab*ac ab*ac))
                     (t2 (/ (r. (rc ap ac) ab*ac) l))
                     (t3 (/ (r. (rc ab ap) ab*ac) l)))
                ;; test if point is in triangle
                (when (and (<= 0 t2 1)
                           (<= 0 t3 1)
                           (<= (+ t2 t3) 1))
                  ;; count # of hits behind ray
                  (incf found-behind)
                  ;; and remember best hit behind ray
                  (when (< t1 d-)
                    (setf d- t1)
                    (v<- p- (rv p0))
                    (v<- n- (rv ab*ac)))))))))
       o)
      (cond
        ((and found (not (zerop found-behind)))
         (assert (= found-behind 1))
         (values p- (nvunit n) T))
        (found
         (values p (nvunit n) NIL))))))
