(in-package #:org.shirakumo.fraf.trial)

(declaim (inline %ray))
(defstruct (ray
            (:constructor %ray (location direction collision-mask ignore))
            (:copier NIL)
            (:predicate NIL))
  (location (vec3) :type vec3)
  (direction (vec3) :type vec3)
  (collision-mask 1 :type (unsigned-byte 32))
  (ignore NIL :type T))

(define-accessor-delegate-methods location (ray-location ray))
(define-accessor-delegate-methods direction (ray-direction ray))

(defmethod collision-mask ((ray ray))
  (ray-collision-mask ray))

(defmethod (setf collision-mask) ((mask integer) (ray ray))
  (setf (ray-collision-mask ray) mask))

(defun ray (location direction &key (collision-mask 1) ignore)
  (%ray location (vunit direction) collision-mask ignore))

(define-compiler-macro ray (location direction &key (collision-mask 1) ignore &environment env)
  `(%ray ,(if (constantp location env) `(load-time-value ,location) location)
         ,(if (constantp direction env) `(load-time-value (vunit ,direction)) `(vunit ,direction))
         ,collision-mask
         ,ignore))

(defmethod print-object ((ray ray) stream)
  (prin1 `(ray ,(location ray) ,(direction ray) ,(collision-mask ray)) stream))

(defun copy-ray (ray)
  (ray (vcopy (ray-location ray))
       (vcopy (ray-direction ray))
       :collision-mask (ray-collision-mask ray)
       :ignore (ray-ignore ray)))

(defun %ray-hit-inner (thunk a b hits start end)
  (declare (type (function (T T T) (or null single-float)) thunk))
  (declare (type ray a))
  (declare (type (unsigned-byte 32) start end))
  (declare (type (simple-vector) hits))
  (when (<= end start)
    (return-from %ray-hit-inner start))
  (when (or (eql (ray-ignore a) (primitive-entity b))
            (= 0 (logand (ray-collision-mask a) (collision-mask b)
                         (collision-mask (primitive-entity b)))))
    (return-from %ray-hit-inner start))
  (let ((hit (aref hits start))
        (ray-location (vec3))
        (ray-direction (vec3)))
    (declare (dynamic-extent ray-location ray-direction))
    (v<- ray-direction (ray-direction a))
    (v<- ray-location (ray-location a))
    ;; Bring the ray into the local transform space of the primitive
    (let ((local (mat4)))
      (declare (dynamic-extent local))
      (!minv local (primitive-transform b))
      (n*m4/3 local ray-direction)
      (n*m local ray-location))
    ;; We have to renormalise in case the transform has scaling.
    (let ((transform-scaling (vlength ray-direction)))
      (nv/ ray-direction transform-scaling)
      (let ((tt (funcall thunk ray-location ray-direction hit)))
        (when tt
          ;; We have to use the world-space ray location and direction here, and thus also
          ;; multiply the time by the transform-scaling to ensure we get the time dilation
          ;; induced by the primitive's transform scaling sorted out.
          (setf (hit-depth hit) (- (* tt transform-scaling)))
          (!v+* (hit-location hit) (ray-location a) (ray-direction a) (* tt transform-scaling))
          (setf (hit-a hit) a)
          (setf (hit-b-detail hit) b)
          (setf (hit-b hit) (primitive-entity b))
          ;; Bring the normal back into global space
          (n*m4/3 (primitive-transform b) (hit-normal hit))
          (incf start))))
    start))

(defun raycast (source direction &key (ignore source) (collision-mask 1) (target (scene +main+)) (hit (make-hit)))
  (let* ((dir (vunit direction))
         (ray (%ray (location source) dir collision-mask ignore))
         (hits (make-array 32))
         (%hit (make-hit)))
    ;; FIXME: this kinda sucks.
    (declare (dynamic-extent dir ray hits %hit))
    (dotimes (i (length hits))
      (setf (aref hits i) %hit)
      (setq %hit (make-hit)))
    (with-ignored-errors-on-release (:trial.physics "Raycast failed, ignoring")
      (let ((count (detect-hits ray target hits 0 (length hits))))
        (when (< 0 count)
          (when (= count (length hits))
            (dbg "RAY overflow"))
          ;; Find the best hit. Ideally we'd feed this back into the broad-phase
          ;; to tell it to stop trying to find shit that's out of reach anyway,
          ;; but....
          (setf (hit-depth hit) most-negative-single-float)
          (dotimes (i count hit)
            (when (< (hit-depth hit) (hit-depth (aref hits i)))
              (<- hit (aref hits i))
              (setf (hit-a hit) NIL))))))))

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
         (flet ((inner (ray-location ray-direction hit)
                  (,implicit-name ray-location ray-direction ,@(if props
                                                                   (loop for prop in props
                                                                         collect `(,(or (third prop) (first prop)) b))
                                                                   (list 'b))
                                  (hit-normal hit))))
           (declare (dynamic-extent #'inner))
           (%ray-hit-inner #'inner a b hits start end))))))

(defmethod detect-hits (a (b ray) hits start end)
  (detect-hits b a hits start end))

(defmethod detect-hits ((a ray) (b 3ds:container) hits start end)
  (declare (type (unsigned-byte 32) start end))
  (declare (type (simple-vector #.(1- (ash 1 32)))))
  (3ds:do-intersecting (element b (ray-location a) (ray-direction a) start)
    (let ((new (detect-hits a element hits start end)))
      (setf start (prune-hits hits start new)))
    (when (<= end start)
      (return-from detect-hits start))))

(defmethod detect-hits ((a ray) (b entity) hits start end)
  (if (next-method-p)
      (call-next-method)
      start))

(define-ray-test sphere ((sphere-radius single-float))
  (let* ((em ray-location)
         (eb (v. em ray-direction))
         (ec (- (v. em em) (* sphere-radius sphere-radius))))
    (unless (and (< 0 ec) (< 0 eb))
      (let ((discriminant (- (* eb eb) ec)))
        (when (<= 0.0 discriminant)
          (let ((tt (- (- eb) (sqrt discriminant))))
            (!v+* ray-normal ray-location ray-direction tt)
            (nvunit* ray-normal)
            tt))))))

(define-ray-test plane ((plane-normal vec3) (plane-offset single-float))
  (let ((denom (v. plane-normal ray-direction)))
    (when (/= 0.0 denom)
      (let ((tt (/ (- plane-offset (v. plane-normal ray-location)) denom)))
        (when (<= 0.0 tt)
          (!v* ray-normal plane-normal (- (float-sign denom)))
          tt)))))

(define-ray-test half-space ((plane-normal vec3) (plane-offset single-float))
  (let ((denom (v. plane-normal ray-direction)))
    (when (/= 0.0 denom)
      (let ((tt (/ (- plane-offset (v. plane-normal ray-location)) denom))
            (tmp (vec3)))
        (declare (dynamic-extent tmp))
        (nv+ (!v* tmp plane-normal (- plane-offset)) ray-location)
        (when (or (<= 0.0 tt) (<= (v. plane-normal tmp) 0.0))
          (v<- ray-normal plane-normal)
          tt)))))

(define-ray-test all-space ()
  (vsetf ray-normal 0 1 0)
  0.0)

(define-ray-test box ((box-bsize vec3))
  ;; Since the ray variables are within local space of the box, the box is
  ;; axis-aligned relative to the ray, so we can simplify the test to an AABB case.
  (flet ((/* (a)
           (declare (type single-float a))
           (if (<= (abs a) 0.0000001f0) 1000000000000.0f0 (/ a))))
    (let* ((inv (vec (/* (vx ray-direction))
                     (/* (vy ray-direction))
                     (/* (vz ray-direction))))
           (t0 (nv* (nv- (v- box-bsize) ray-location) inv))
           (t1 (nv* (nv- (v+ box-bsize) ray-location) inv))
           (t< (vmin t0 t1))
           (t> (vmax t0 t1))
           (tmin (max (vx t<) (vy t<) (vz t<)))
           (tmax (min (vx t>) (vy t>) (vz t>))))
      (declare (dynamic-extent inv t0 t1 t< t>))
      (when (< tmin tmax)
        (cond ((and (< (vy t<) (vx t<)) (< (vz t<) (vx t<)))
               (vsetf ray-normal (float-sign (vx ray-location)) 0 0))
              ((and (< (vx t<) (vy t<)) (< (vz t<) (vy t<)))
               (vsetf ray-normal 0 (float-sign (vy ray-location)) 0))
              (T
               (vsetf ray-normal 0 0 (float-sign (vz ray-location)))))
        tmin))))

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

;; FIXME: implement ray-cone test
#++
(define-ray-test cone ((cone-height single-float) (cone-radius single-float))
  )

(define-ray-test cylinder ((cylinder-height single-float) (rb single-float cylinder-radius-bottom) (rt single-float cylinder-radius-top))
  ;; Note: the cylinder is always centred around 0,0,0 and oriented along Y.
  ;;       As such we can simplify the logic here.
  ;; FIXME: fix for different radii
  (let* ((r (max rb rt))
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

(define-ray-test pill ((h single-float pill-height) (rb single-float pill-radius-bottom) (rt single-float pill-radius-top))
  (if (= 0.0 h)
      (ray-sphere-p ray-location ray-direction (max rb rt) ray-normal)
      (or (let ((tt (ray-cylinder-p ray-location ray-direction h rb rt ray-normal)))
            (when (and tt ;; Ignore cap hits
                       (or (/= 0.0 (vx ray-normal))
                           (/= 1.0 (abs (vy ray-normal)))
                           (/= 0.0 (vz ray-normal))))
              tt))
          (let ((top-ray (vec3 (vx ray-location)
                               (- (vy ray-location) h)
                               (vz ray-location))))
            (declare (dynamic-extent top-ray))
            (ray-sphere-p top-ray ray-direction rt ray-normal))
          (let ((bottom-ray (vec3 (vx ray-location)
                                  (+ (vy ray-location) h)
                                  (vz ray-location))))
            (declare (dynamic-extent bottom-ray))
            (ray-sphere-p bottom-ray ray-direction rb ray-normal)))))
