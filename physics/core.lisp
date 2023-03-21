(in-package #:org.shirakumo.fraf.trial)

(defgeneric integrate (object dt))
(defgeneric mass (object))
(defgeneric (setf mass) (mass object))
(defgeneric start-frame (object))

(defclass physics-entity ()
  ((velocity :initform (vec 0 0 0) :reader velocity)
   (inverse-mass :initform 0.0 :accessor inverse-mass)
   (force :initform (vec 0 0 0) :reader force)
   (damping :initform 0.95 :accessor damping)
   (awake-p :initform T :accessor awake-p)
   (%motion :initform most-positive-single-float :accessor %motion)))

(defmethod shared-initialize :after ((entity physics-entity) slots &key mass)
  (when mass (setf (mass entity) mass)))

(defmethod (setf velocity) ((vel vec3) (entity physics-entity))
  (v<- (velocity entity) vel))

(defmethod (setf force) ((val vec3) (entity physics-entity))
  (v<- (force entity) val))

(defmethod mass ((entity physics-entity))
  (let ((inverse (inverse-mass entity)))
    (if (= 0.0 inverse)
        0.0
        (/ (inverse-mass entity)))))

(defmethod (setf mass) (mass (entity physics-entity))
  (setf (inverse-mass entity) (if (= 0.0 mass) 0.0 (abs (float (/ mass) 0.0)))))

(defmethod (setf awake-p) :after (status (entity physics-entity))
  (if status
      (setf (%motion entity) most-positive-single-float)
      (vsetf (velocity entity) 0 0 0)))

(defmethod current-motion ((entity physics-entity))
  (v. (velocity entity) (velocity entity)))

(defclass force ()
  ())

(defgeneric apply-force (force particle dt))

(defmethod apply-force :around ((force force) (entity physics-entity) dt)
  (when (< 0 (inverse-mass entity))
    (call-next-method)))

(defclass gravity (force)
  ((gravity :initarg :gravity :initform (vec 0 -10 0) :accessor gravity)))

(defmethod apply-force ((force gravity) (entity physics-entity) dt)
  (nv+* (force entity) (gravity force) (mass entity)))

(defclass drag-force (force)
  ((k1 :initarg :k1 :initform 1.0 :accessor k1)
   (k2 :initarg :k2 :initform 1.0 :accessor k2)))

(defmethod apply-force ((force drag-force) (entity physics-entity) dt)
  (let* ((force (vcopy (velocity entity)))
         (coeff (vlength force))
         (coeff (+ (* (k1 force) coeff) (* (k2 force) coeff coeff))))
    (nvunit force)
    (nv* force (- coeff))
    (nv+ (force entity) force)))

(defclass spring-force (force)
  ((anchor :initarg :anchor :accessor anchor)
   (anchor-offset :initarg :anchor-offset :initform (vec 0 0 0) :accessor anchor-offset)
   (local-offset :initarg :local-offset :initform (vec 0 0 0) :accessor local-offset)
   (spring-constant :initarg :spring-constant :initform 1.0 :accessor spring-constant)
   (rest-length :initarg :rest-length :initform 0.0 :accessor rest-length)))

(defmethod apply-force ((force spring-force) (entity physics-entity) dt)
  (let* ((force (v- (location entity) (location (anchor force))))
         (coeff (* (abs (- (vlength force) (rest-length force)))
                   (spring-constant force))))
    (nv+ (force entity) (nv* (nvunit force) (- coeff)))))

(defclass stiff-spring-force (force)
  ((anchor :initarg :anchor :accessor anchor)
   (anchor-offset :initarg :anchor-offset :initform (vec 0 0 0) :accessor anchor-offset)
   (local-offset :initarg :local-offset :initform (vec 0 0 0) :accessor local-offset)
   (spring-constant :initarg :spring-constant :initform 1.0 :accessor spring-constant)
   (damping :initarg :damping :initform 1.0 :accessor damping)))

(defmethod apply-force ((force stiff-spring-force) (entity physics-entity) dt)
  (let* ((damping (damping force))
         (relative (v- (location entity) (location (anchor force))))
         (gamma (* 0.5 (sqrt (- (* 4 (spring-constant force)) (* damping damping)))))
         (c (nv+ (v* relative (/ damping (* 2 gamma)))
                 (v* (velocity entity) (/ gamma))))
         (target (nv* (nv+ (v* relative (cos (* gamma dt)))
                           (v* c (sin (* gamma dt))))
                      (exp (* -0.5 damping dt))))
         (accel (nv- (nv* (v- target relative) (/ (* dt dt)))
                     (v* (velocity entity) dt))))
    (nv+ (force entity) (nv* accel (mass entity)))))

(defclass bungee-force (spring-force)
  ())

(defmethod apply-force ((force bungee-force) (entity physics-entity) dt)
  (let* ((force (v- (location entity) (location (anchor force))))
         (coeff (* (- (vlength force) (rest-length force))
                   (spring-constant force))))
    (when (<= 0.0 coeff)
      (nv+ (force entity) (nv* (nvunit force) (- coeff))))))

(defclass located-force (force)
  ((location :initform (vec 0 0 0) :initarg :location :reader location)))

(defmethod (setf location) ((vec vec3) (obj located-force))
  (v<- (location obj) vec))

(defclass spherical-force (located-force)
  ((radius :initform 1.0 :initarg :radius :accessor radius)))

(defmethod apply-force :around ((force spherical-force) (entity located-entity) dt)
  (when (<= (vdistance (location force) (location entity)) (radius force))
    (call-next-method)))

(defclass aabb-force (located-force)
  ((bsize :initform (vec 0 0 0) :initarg :bsize :accessor bsize)))

(defmethod (setf bsize) ((vec vec3) (obj aabb-force))
  (v<- (bsize obj) vec))

(defmethod apply-force :around ((force aabb-force) (entity located-entity) dt)
  (when (v<= (nvabs (v- (location entity) (location force)))
             (v+ (bsize force) (bsize entity)))
    (call-next-method)))

(defstruct hit
  (a NIL :type T)
  (b NIL :type T)
  (location (vec 0 0 0) :type vec3)
  (normal (vec 0 0 0) :type vec3)
  (restitution 0.0 :type single-float)
  (static-friction 0.0 :type single-float)
  (dynamic-friction 0.0 :type single-float)
  (depth 0.0 :type single-float))

(define-accessor-delegate-methods a (hit-a hit))
(define-accessor-delegate-methods b (hit-b hit))
(define-accessor-delegate-methods location (hit-location hit))
(define-accessor-delegate-methods normal (hit-normal hit))
(define-accessor-delegate-methods restitution (hit-restitution hit))
(define-accessor-delegate-methods static-friction (hit-static-friction hit))
(define-accessor-delegate-methods dynamic-friction (hit-dynamic-friction hit))
(define-accessor-delegate-methods depth (hit-depth hit))

(defclass physics-system (entity listener)
  ((forces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor forces)
   (%objects :initform (make-array 0 :adjustable T :fill-pointer T) :accessor %objects)
   (hits :initform (map-into (make-array 128) #'make-hit) :accessor hits)
   (sleep-eps :initform 0.3 :initarg :sleep-eps :accessor sleep-eps)))

(defmethod shared-initialize :after ((system physics-system) slots &key units-per-metre)
  (when units-per-metre (setf (units-per-metre system) units-per-metre)))

(defmethod (setf units-per-metre) (units (system physics-system))
  ;; The default we pick here is for assuming 1un = 1cm
  (setf (sleep-eps system) (* units 0.3)))

(defgeneric generate-hits (system hits start end))
(defgeneric resolve-hits (system hits start end dt &key))

(defmethod enter (thing (system physics-system))
  (vector-push-extend thing (%objects system))
  thing)

(defmethod enter ((thing force) (system physics-system))
  (vector-push-extend thing (forces system))
  thing)

(defmethod leave (thing (system physics-system))
  (array-utils:vector-pop-position (%objects system)
                                   (position thing (%objects system)))
  thing)

(defmethod leave ((thing force) (system physics-system))
  (array-utils:vector-pop-position (forces system)
                                   (position thing (forces system)))
  thing)

(defmethod integrate ((system physics-system) dt)
  (loop with bias = (expt 0.5 dt)
        with sleep-eps = (sleep-eps system)
        for entity across (%objects system)
        do (when (awake-p entity)
             (integrate entity dt)
             (let* ((bias (expt 0.5 dt))
                    (current (current-motion entity))
                    (motion (+ (* bias (%motion entity))
                               (* (- 1 bias) current))))
               (cond ((< motion sleep-eps)
                      (setf (awake-p entity) NIL))
                     ((< (* 10 sleep-eps) motion)
                      (setf (%motion entity) (* 10 sleep-eps)))
                     (T
                      (setf (%motion entity) motion)))))))

(defmethod start-frame ((system physics-system))
  (loop for entity across (%objects system)
        when (awake-p entity)
        do (start-frame entity)))

(defmethod update :before ((system physics-system) tt dt fc)
  (start-frame system))

(defmethod update ((system physics-system) tt dt fc)
  (loop for entity across (%objects system)
        when (awake-p entity)
        do (loop for force across (forces system)
                 do (apply-force force entity dt)))
  (integrate system dt)
  (let* ((hits (hits system))
         (end (generate-hits system hits 0 (length hits))))
    (when (< 0 end)
      (resolve-hits system hits 0 end dt))))

(define-handler (physics-system tick) (tt dt fc)
  (update physics-system tt dt fc))
