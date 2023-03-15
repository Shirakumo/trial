(in-package #:org.shirakumo.fraf.trial)

(defgeneric integrate (object dt))
(defgeneric mass (object))
(defgeneric (setf mass) (mass object))

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

(defclass stiff-spring-force (force)
  ((anchor :initarg :anchor :accessor anchor)
   (anchor-offset :initarg :anchor-offset :initform (vec 0 0 0) :accessor anchor-offset)
   (local-offset :initarg :local-offset :initform (vec 0 0 0) :accessor local-offset)
   (spring-constant :initarg :spring-constant :initform 1.0 :accessor spring-constant)
   (damping :initarg :damping :initform 1.0 :accessor damping)))

(defclass bungee-force (spring-force)
  ())

(defstruct hit
  (a NIL :type T)
  (b NIL :type T)
  (location (vec 0 0 0) :type vec3)
  (normal (vec 0 0 0) :type vec3)
  (restitution 0.0 :type single-float)
  (static-friction 0.0 :type single-float)
  (dynamic-friction 0.0 :type single-float)
  (depth 0.0 :type single-float))

(defclass physics-system ()
  ((forces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor forces)
   (%objects :initform (make-array 0 :adjustable T :fill-pointer T) :accessor %objects)
   (sleep-eps :initform 0.3 :initarg :sleep-eps :accessor sleep-eps)))

(defmethod shared-initialize :after ((system physics-system) slots &key units-per-metre)
  (when units-per-metre (setf (units-per-metre system) units-per-metre)))

(defmethod (setf units-per-metre) (units (system physics-system))
  ;; The default we pick here is for assuming 1un = 1cm
  (setf (sleep-eps system) (* units 0.3)))

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

(defmethod update ((system physics-system) tt dt fc)
  (start-frame system)
  (loop for entity across (%objects system)
        when (awake-p entity)
        do (loop for force across (forces system)
                 do (apply-force force entity dt)))
  (integrate system dt))
