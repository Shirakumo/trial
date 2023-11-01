(in-package #:org.shirakumo.fraf.trial)

(declaim (inline %ray))
(defstruct (ray
            (:constructor %ray (location direction))
            (:copier NIL)
            (:predicate NIL))
  (location (vec3) :type vec3)
  (direction (vec3) :type vec3))

(define-accessor-delegate-methods location (ray-location ray))
(define-accessor-delegate-methods direction (ray-direction ray))

(defun ray (&optional (location (vec3)) (direction +vz+))
  (%ray location (vunit direction)))

(define-compiler-macro ray (&optional (location '(vec3)) (direction '+vz+) &environment env)
  `(%ray ,(if (constantp location env) `(load-time-value ,location) location)
         ,(if (constantp direction env) `(load-time-value (vunit ,direction)) `(vunit ,direction))))

(defmethod print-object ((ray ray) stream)
  (prin1 `(ray ,(location ray) ,(direction ray)) stream))

(defun copy-ray (ray)
  (ray (vcopy (ray-location ray))
       (vcopy (ray-direction ray))))

(defmacro define-ray-test (b (&rest props) &body body)
  (let ((implicit-name (intern (format NIL "~a-~a-~a" 'ray b 'p)))
        (loc (intern (string '#:ray-location)))
        (dir (intern (string '#:ray-direction)))
        (normal (intern (string '#:ray-normal)))
        (props (mapcar #'enlist props)))
    `(progn
       ;; The implicit function is useful to handle composite tests where we depend on the
       ;; result of another shape in some fashion. All implicit functions assume the other
       ;; shape is centred at the origin and axis-aligned, as we handle those kinds of
       ;; shifts using the primitive's transform in the DETECT-HITS function.
       (declaim (ftype (function (vec3 vec3 ,@(if props (mapcar #'second props) (list b)) &optional vec3)
                                 (or null single-float)) ,implicit-name))
       (defun ,implicit-name (,loc ,dir ,@(if props (mapcar #'first props) (list b)) &optional (,normal (vec3)))
         (declare (optimize speed (safety 1))
                  (type vec3 ,loc ,dir ,normal)
                  ,@(loop for (prop type) in props
                          when type
                          collect `(type ,type ,prop))
                  (ignorable ,normal))
         (block NIL
           ,@body))
       
       (defmethod detect-hits ((a ray) (b ,b) hits start end)
         (declare (type (unsigned-byte 32) start end))
         (declare (type (simple-vector ,(1- (ash 1 32)))))
         (when (<= end start)
           (return-from detect-hits start))
         (let ((hit (aref hits start))
               (ray-location (vec3))
               (ray-direction (vec3)))
           (declare (dynamic-extent ray-location ray-direction))
           (v<- ray-location (ray-location a))
           (v<- ray-direction (ray-direction a))
           ;; Bring the ray into the local transform space of the primitive
           (n*m (primitive-transform b) ray-location)
           ;; We apply this weird transform to avoid translation on the ray direction
           (n*m4/3 (primitive-transform b) ray-direction)
           ;; We have to renormalise in case the transform has scaling.
           (let ((transform-scaling (vlength ray-direction)))
             (nv/ ray-direction transform-scaling)
             (block NIL
               (let ((tt (,implicit-name ray-location ray-direction ,@ (if props
                                                                           (loop for prop in props
                                                                                 collect `(,(first prop) b))
                                                                           (list b))
                                         (hit-normal hit))))
                 (when tt
                   (v<- (hit-location hit) (ray-location a))
                   ;; We have to use the world-space ray location and direction here, and thus also
                   ;; multiply the time by the transform-scaling to ensure we get the time dilation
                   ;; induced by the primitive's transform scaling sorted out.
                   (nv+* (hit-location hit) (ray-direction a) (max 0.0 (* tt transform-scaling)))
                   (setf (hit-a hit) a)
                   (setf (hit-b hit) ,(if (subtypep b 'primitive) `(primitive-entity b) b))
                   (ntransform-inverse (hit-normal hit) (primitive-transform b))
                   (incf start)))))
           start))
       
       (defmethod detect-hits ((a ,b) (b ray) hits start end)
         (detect-hits b a hits start end)))))

(defmethod detect-hits ((a ray) (b 3ds:container) hits start end)
  (declare (type (unsigned-byte 32) start end))
  (declare (type (simple-vector #.(1- (ash 1 32)))))
  (3ds:do-intersecting (element b (ray-location a) (ray-direction a))
    (when (<= end start)
      (return-from detect-hits start))
    (setf start (detect-hits a element hits start end))))

(define-ray-test plane ((plane-normal vec3) (plane-offset single-float))
  (let ((denom (v. plane-normal ray-direction)))
    (when (/= 0.0 denom)
      (let ((tt (/ (- plane-offset (v. plane-normal ray-location)) denom)))
        (when (<= 0.0 tt) 
          (v<- ray-normal plane-normal)
          tt)))))

(define-ray-test sphere ((sphere-radius single-float))
  (let* ((em ray-location)
         (eb (v. em ray-direction))
         (ec (- (v. em em) (* sphere-radius sphere-radius))))
    (unless (and (< 0 ec) (< 0 eb))
      (let ((discriminant (- (* eb eb) ec)))
        (when (<= 0.0 discriminant)
          (let ((tt (- (- eb) (sqrt discriminant))))
            (v<- ray-normal ray-location)
            (nv+* ray-normal ray-direction tt)
            (nvunit ray-normal)
            tt))))))

(define-ray-test box ((box-bsize vec3))
  ;; Since the ray variables are within local space of the box, the box is
  ;; axis-aligned relative to the ray, so we can simplify the test to an AABB case.
  (let ((tmin 0.0)
        (tmax most-positive-single-float))
    ;; The box constitutes three "slabs" with each right-angle face pair. We only
    ;; need to know if the ray is within all three slabs at the same time, which we
    ;; do here.
    (macrolet ((test (axis)
                 `(cond ((< (abs (,axis ray-direction)) SINGLE-FLOAT-EPSILON)
                         (when (or (< (,axis ray-location) (- (,axis box-bsize)))
                                   (< (,axis box-bsize) (,axis ray-direction)))
                           (return)))
                        (T
                         (let* ((ood (/ (,axis ray-direction)))
                                (t1 (* ood (- (- (,axis box-bsize)) (,axis ray-location))))
                                (t2 (* ood (- (+ (,axis box-bsize)) (,axis ray-location))))
                                (d 1.0))
                           (if (< t2 t1)
                               (rotatef t1 t2)
                               (setf d -1.0))
                           (when (< tmin t1)
                             (setf tmin t1)
                             (vsetf ray-normal 0 0 0)
                             (setf (,axis ray-normal) d))
                           (when (< t2 tmax)
                             (setf tmax t2))
                           (when (< tmax tmin)
                             (return)))))))
      (test vx3)
      (test vy3)
      (test vz3))
    (when (/= tmax most-positive-single-float)
      tmin)))

(define-ray-test triangle ((triangle-a vec3) (triangle-b vec3) (triangle-c vec3))
  (let* ((p ray-location)
         (a triangle-a)
         (b triangle-b)
         (c triangle-c)
         (ab (vec3))
         (ac (vec3))
         (e (vec3))
         (qp ray-direction))
    (declare (dynamic-extent ab ac e))
    (nv- (v<- ab b) a)
    (nv- (v<- ac c) a)
    (let* ((n (!vc ray-normal ab ac))
           (d (v. qp n)))
      ;; If we're parallel, exit early.
      (when (<= d 0.0) (return))
      ;; Compute the intersection of the PQ plane
      (let* ((ap (v- p a))
             (tt (v. ap n)))
        (when (< tt 0.0) (return))
        ;; Compute the barycentric coordinates and test bounds
        (let* ((e (!vc e qp ap))
               (v (v. ac e))
               (w (- (v. ab e))))
          (when (or (< v 0.0) (< v d)) (return))
          (when (or (< w 0.0) (< (+ v w) d)) (return))
          (/ tt d))))))

(define-ray-test cylinder ((cylinder-height single-float) (cylinder-radius single-float))
  ;; Note: the cylinder is always centred around 0,0,0 and oriented along Y.
  ;;       As such we can simplify the logic here.
  (let* ((r cylinder-radius)
         (m (vec3 (vx ray-location) (+ (vy ray-location) cylinder-height) (vz ray-location)))
         (n ray-direction)
         (md (* 2 cylinder-height (vy m)))
         (nd (* 2 cylinder-height (vy n)))
         (dd (* 2 cylinder-height 2 cylinder-height)))
    (declare (dynamic-extent m))
    ;; Check if outside caps
    (when (and (< md 0.0) (< (+ md nd) 0.0)) (return))
    (when (and (< dd md) (< dd (+ md nd))) (return))
    (let* ((nn (v. n n))
           (mn (v. m n))
           (a (- (* dd nn) (* nd nd)))
           (k (- (v. m m) (* r r)))
           (c (- (* dd k) (* md md))))
      (when (<= (abs a) SINGLE-FLOAT-EPSILON)
        ;; We're parallel, so check if outside the cylinder
        (when (< 0.0 c) (return))
        (return (cond ((< md 0.0) (/ (- mn) nn))   ;; -Y endcap
                      ((< dd md) (/ (- nd mn) nn)) ;; +Y endcap
                      (T 0.0))))                   ;; Origin inside
      (let* ((b (- (* dd mn) (* nd md)))
             (discriminant (- (* b b) (* a c))))
        (when (< discriminant 0.0) (return))
        (let ((tt (/ (- (- b) (sqrt discriminant)) a)))
          (when (< tt 0.0) (return)) ;; Behind ray
          (cond ((< (+ md (* tt nd)) 0.0)
                 ;; Check against -Y endcap
                 (when (<= nd 0.0) (return)) ;; pointing away from cap
                 (setf tt (/ (- md) nd))
                 (when (<= (+ k (* 2 tt (+ mn (* tt nn)))) 0.0)
                   (vsetf ray-normal 0 -1 0)
                   tt))
                ((< dd (+ md (* tt nd)))
                 ;; Check against +Y endcap
                 (when (<= 0.0 nd) (return))
                 (setf tt (/ (- dd md) nd))
                 (when (<= (+ k dd (* -2 md) (* tt (+ (* 2 (- mn nd)) (* tt nn)))) 0.0)
                   (vsetf ray-normal 0 +1 0)
                   tt))
                (T
                 ;; We hit the cylinder body
                 (v<- ray-normal ray-location)
                 (nv+* ray-normal ray-direction tt)
                 (setf (vy ray-normal) 0.0)
                 tt)))))))

(define-ray-test pill ((pill-height single-float) (pill-radius single-float))
  (cond ((= 0.0 pill-height)
         (ray-sphere-p ray-location ray-direction pill-radius ray-normal))
        (T
         (or (let ((tt (ray-cylinder-p ray-location ray-direction pill-height pill-radius ray-normal)))
               (when (and tt ;; Ignore cap hits
                          (or (/= 0.0 (vx ray-normal))
                              (/= 1.0 (abs (vy ray-normal)))
                              (/= 0.0 (vz ray-normal))))
                 tt))
             (let ((top-ray (vec3 (vx ray-location)
                                  (- (vy ray-location) pill-height)
                                  (vz ray-location))))
               (declare (dynamic-extent top-ray))
               (ray-sphere-p top-ray ray-direction pill-radius ray-normal))
             (let ((bottom-ray (vec3 (vx ray-location)
                                     (+ (vy ray-location) pill-height)
                                     (vz ray-location))))
               (declare (dynamic-extent bottom-ray))
               (ray-sphere-p bottom-ray ray-direction pill-radius ray-normal))))))
