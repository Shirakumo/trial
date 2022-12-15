(in-package #:org.shirakumo.fraf.trial)

(defgeneric integrate (object dt))
(defgeneric mass (object))
(defgeneric (setf mass) (object))

(defclass physics-entity ()
  ((velocity :initform (vec 0 0 0) :reader velocity)
   (inverse-mass :initform 0.0 :accessor inverse-mass)
   (force :initform (vec 0 0 0) :reader force)
   (damping :initform 1.0 :accessor damping)))

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

(defclass force ()
  ())

(defgeneric apply-force (force particle dt))

(defmethod apply-force :around ((force force) (entity physics-entity) dt)
  (when (< 0 (inverse-mass entity))
    (call-next-method)))

(defclass gravity (force)
  ((gravity :initarg :gravity :initform (vec 0 -1 0) :accessor gravity)))

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
  (restitution 0.0 :type single-float)
  (normal (vec 0 0 0) :type vec3)
  (depth 0.0 :type single-float))

(defclass hit-generator () ())

(defgeneric generate-hits (generator hits start end))

(defmacro define-hit-generation (generator &body body)
  `(defmethod generate-hits (,(enlist generator generator) hits start end)
     (let ((hit (aref hits start)))
       (block NIL
         (flet ((finish-hit ()
                  (incf start)
                  (if (< start end)
                      (setf hit (aref hits start))
                      (return))))
           ,@body))
       start)))

(defclass physics-system (container)
  ((forces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor forces)))

(defmethod integrate ((system physics-system) dt)
  (sequences:dosequence (entity system)
    (integrate entity dt)))

(defmethod start-frame ((system physics-system))
  (sequences:dosequence (entity system)
    (start-frame entity)))

(defmethod update ((system rigidbody-system) tt dt fc)
  (sequences:dosequence (entity system)
    (loop for force across (forces system)
          do (apply-force force entity dt)))
  (integrate system dt))
