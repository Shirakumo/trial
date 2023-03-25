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
  (ray (vcopy3 (ray-location ray))
       (vcopy3 (ray-direction ray))))

(defmacro define-ray-test ((b &rest props) &body body)
  (let ((implicit-name (intern (format NIL "~a-~a-~a" 'ray b 'p))))
    (form-fiddle:with-body-options (body other normal) body
      (declare (ignore other))
      `(progn
         ;; The implicit function is useful to handle composite tests where we depend on the
         ;; result of another shape in some fashion. All implicit functions assume the other
         ;; shape is centred at the origin and axis-aligned, as we handle those kinds of
         ;; shifts using the primitive's transform in the DETECT-HITS function.
         (defun ,implicit-name (ray-location ray-direction ,@(mapcar #'first props))
           (declare (optimize speed (safety 1))
                    (type vec3 ray-location ray-direction)
                    ,@(loop for (prop type) in props
                            collect `(type ,type ,prop)))
           (block NIL
             ,@body))
         
         (defmethod detect-hits ((a ray) (b ,b) hits start end)
           (declare (type (unsigned-byte 32) start end))
           (declare (type (simple-vector ,(1- (ash 1 32)))))
           (when (<= end start)
             (return-from detect-hits start))
           (let ((hit (aref hits start))
                 (ray-location (vec3 0 0 0))
                 (ray-direction (vec3 0 0 0)))
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
                 (let ((tt (,implicit-name ray-location ray-direction ,@(loop for prop in props
                                                                              collect `(,(first prop) b)))))
                   (when tt
                     (v<- (hit-location hit) (ray-location a))
                     ;; We have to use the world-space ray location and direction here, and thus also
                     ;; multiply the time by the transform-scaling to ensure we get the time dilation
                     ;; induced by the primitive's transform scaling sorted out.
                     (nv+* (hit-location hit) (ray-direction a) (max 0.0 (* tt transform-scaling)))
                     (setf (hit-a hit) a)
                     (setf (hit-b hit) ,(if (subtypep b 'primitive) `(primitive-entity b) NIL))
                     ,normal
                     (incf start)))))
             start))
         
         (defmethod detect-hits ((a ,b) (b ray) hits start end)
           (detect-hits b a hits start end))))))

(define-ray-test (plane (plane-normal vec3) (plane-offset single-float))
  :normal (v<- (hit-normal hit) (plane-normal b))
  (let ((denom (v. plane-normal ray-direction)))
    (when (/= 0.0 denom)
      (let ((tt (/ (- plane-offset (v. plane-normal ray-location)) denom)))
        (when (<= 0.0 tt) tt)))))

(define-ray-test (sphere (sphere-radius single-float))
  :normal (nvunit (nv- (v<- (hit-normal hit) (hit-location hit)) (location b)))
  (let* ((em ray-location)
         (eb (v. em ray-direction))
         (ec (- (v. em em) (* sphere-radius sphere-radius))))
    (unless (and (< 0 ec) (< 0 eb))
      (let ((discriminant (- (* eb eb) ec)))
        (when (<= 0.0 discriminant)
          (- (- eb) (sqrt discriminant)))))))

(define-ray-test (box (box-bsize vec3))
  :normal (vsetf (hit-normal hit) 0 0 0) ;; TODO: implement normal
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
                                   (< (+ (,axis box-bsize) (,axis ray-direction))))
                           (return)))
                        (T
                         (let* ((ood (/ (,axis ray-direction)))
                                (t1 (* ood (- (- (,axis box-bsize)) (,axis ray-location))))
                                (t2 (* ood (- (+ (,axis box-bsize)) (,axis ray-location)))))
                           (when (< t2 t1) (rotatef t1 t2))
                           (setf tmin (max tmin t1))
                           (setf tmax (max tmax t2))
                           (when (< tmax tmin) (return)))))))
      (test vx3)
      (test vy3)
      (test vz3))
    tmin))

(define-ray-test (triangle (triangle-a vec3) (triangle-b vec3) (triangle-c vec3))
  )

(define-ray-test (cylinder (cylinder-height single-float) (cylinder-radius single-float))
  :normal (let ((cylinder-local-hit (nv+* (v<- (hit-normal hit) ray-location) ray-direction tt)))
            ;; We first compute the hit in the cylinder's local space. If the hit is on the height,
            ;; then the normal is parallel to the cylinder. Otherwise, the normal is perpendicular
            ;; and we can thus truncate the Y component.
            (cond ((<= (cylinder-height b) (vy cylinder-local-hit))
                   (vsetf (hit-normal hit) 0 1 0))
                  ((<= (vy cylinder-local-hit) (- (cylinder-height b)))
                   (vsetf (hit-normal hit) 0 -1 0))
                  (T
                   (setf (vy (hit-normal hit)) 0.0)))
            ;; Then we do an inverse transform to get back into world space with the normal.
            (ntransform-inverse (hit-normal hit) (cylinder-transform b)))
  ;; Note: the cylinder is always centred around 0,0,0 and oriented along Y.
  ;;       As such we can simplify the logic here.
  (let* (#|(p (vec3 0 (- cylinder-height) 0))|#
         #|(q (vec3 0 (+ cylinder-height) 0))|#
         (r cylinder-radius)
         #|(sa ray-location)|#
         #|(sb (v+ ray-location ray-direction))|#
         #|(d (vec3 0 (* 2 cylinder-height) 0))|#
         (m (vec3 (vx ray-location) (+ (vy ray-location) cylinder-height) (vz ray-location)))
         (n ray-direction)
         (md (* 2 cylinder-height (vy m)) #|(v. m d)|#)
         (nd (* 2 cylinder-height (vy n)) #|(v. n d)|#)
         (dd (* 2 cylinder-height 2 cylinder-height) #|(v. d d)|#))
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
        (return (cond ((< md 0.0) (/ (- mn) nn))   ;; P endcap
                      ((< dd md) (/ (- nd mn) nn)) ;; Q endcap
                      (T 0.0))))                   ;; Origin inside
      (let* ((b (- (* dd mn) (* nd md)))
             (discriminant (- (* b b) (* a c))))
        (when (< discriminant 0.0) (return))
        (let ((tt (/ (- (- b) (sqrt discriminant)) a)))
          (when (< tt 0.0) (return)) ;; Behind ray
          (cond ((< (+ md (* tt nd)) 0.0)
                 ;; Check against P endcap
                 (when (<= nd 0.0) (return)) ;; pointing away from cap
                 (setf tt (/ (- md) nd))
                 (when (<= (+ k (* 2 tt (+ mn (* tt nn)))) 0.0)
                   (return tt)))
                ((< dd (+ md (* tt nd)))
                 ;; Check against Q endcap
                 (when (<= 0.0 nd) (return))
                 (setf tt (/ (- dd md) nd))
                 (when (<= (+ k dd (* -2 md) (* tt (+ (* 2 (- mn nd)) (* tt nn)))) 0.0)
                   (return tt)))
                (T
                 ;; We hit the cylinder body
                 (return tt))))))))

(define-ray-test (pill (pill-height single-float) (pill-radius single-float))
  :normal (vsetf (hit-normal hit) 0 0 0) ; FIXME: implement normal for pill
  (cond ((= 0.0 pill-height)
         (ray-sphere-p ray-location ray-direction pill-radius))
        (T
         ;; FIXME: This is not correct if we hit the cylinder's cap.
         (or (ray-cylinder-p ray-location ray-direction pill-height pill-radius)
             (let ((top-ray (vec3 (vx ray-location)
                                  (- (vy ray-location) pill-height)
                                  (vz ray-location))))
               (declare (dynamic-extent top-ray))
               (ray-sphere-p top-ray ray-direction pill-radius))
             (let ((bottom-ray (vec3 (vx ray-location)
                                     (+ (vy ray-location) pill-height)
                                     (vz ray-location))))
               (declare (dynamic-extent bottom-ray))
               (ray-sphere-p bottom-ray ray-direction pill-radius))))))
