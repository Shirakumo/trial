(in-package #:org.shirakumo.fraf.trial)

(defclass trigger-volume (rigid-shape basic-node)
  ((active-p :initarg :active-p :initform T :accessor active-p)
   (triggered-p :initform NIL :accessor triggered-p)))

(define-transfer trigger-volume active-p triggered-p)

(defgeneric activate-trigger (target trigger))

(defmethod activate-trigger :after (target (trigger trigger-volume))
  (setf (triggered-p trigger) T))

(defmethod awake-p ((entity trigger-volume))
  NIL)

(defmethod collides-p ((a rigidbody) (trigger trigger-volume) hit)
  (active-p trigger))

(defmethod collides-p ((a trigger-volume) (b rigidbody) hit)
  (collides-p b a (reverse-hit hit)))

(defmethod collides-p :around ((a rigidbody) (trigger trigger-volume) hit)
  (when (call-next-method)
    (activate-trigger a trigger))
  NIL)

(defclass one-shot-trigger-volume (trigger-volume)
  ())

(defmethod activate-trigger :after (a (trigger one-shot-trigger-volume))
  (setf (active-p trigger) NIL))

(defclass class-filtered-trigger-volume (trigger-volume)
  ((class-name :initarg :class-name :initform 'rigid-shape :accessor class-name)))

(define-transfer class-filtered-trigger-volume class-name)

(defmethod collides-p ((a rigid-shape) (trigger class-filtered-trigger-volume) hit)
  (and (typep a (class-name trigger)) (call-next-method)))

(defclass rearming-trigger-volume (trigger-volume listener)
  ((cooldown :initarg :cooldown :initform 1.0 :accessor cooldown)
   (cooldown-timer :initform 0.0 :accessor cooldown-timer)))

(define-transfer rearming-trigger-volume cooldown)

(defmethod activate-trigger :after (a (trigger rearming-trigger-volume))
  (setf (cooldown-timer trigger) 0.0))

(define-handler (rearming-trigger-volume tick) (dt)
  (when (<= (cooldown rearming-trigger-volume) (incf (cooldown-timer rearming-trigger-volume) dt))
    (setf (triggered-p rearming-trigger-volume) NIL)
    (setf (active-p rearming-trigger-volume) T)))

(defclass thunk-trigger-volume (trigger-volume)
  ((thunk :initarg :thunk :accessor thunk)))

(define-transfer thunk-trigger-volume active-p thunk)

(defmethod shared-initialize :after ((trigger thunk-trigger-volume) slots &key form)
  (etypecase form
    (null)
    (function
     (setf (thunk trigger) form))
    (cons
     (setf (thunk trigger) (compile NIL `(lambda (rigid-shape trigger-volume) ,form))))))

(defmethod activate-trigger (a (trigger thunk-trigger-volume))
  (funcall (thunk trigger) a trigger))

(defclass place-trigger-volume (trigger-volume)
  ((setter :initarg :setter :accessor setter)
   (getter :initarg :getter :accessor getter)
   (value :initarg :value :accessor value)
   (action :initarg :action :initform 'setf :accessor action)))

(define-transfer place-trigger-volume setter getter value action)

(defmethod activate-trigger (a (trigger place-trigger-volume))
  (ecase (mode trigger)
    (setf (funcall (setter trigger) (value trigger)))
    (incf (funcall (setter trigger) (+ (funcall (getter trigger)) (value trigger))))
    (decf (funcall (setter trigger) (- (funcall (getter trigger)) (value trigger))))
    (random (funcall (setter trigger) (random (value trigger))))))

(defclass accessor-trigger-volume (place-trigger-volume)
  ((object :initarg :object :reader object)
   (accessor :initarg :accessor :reader accessor)
   (value :initarg :value :accessor value)
   (action :initarg :action :initform 'setf :accessor action)))

(define-transfer accessor-trigger-volume object accessor value action)

(defmethod shared-initialize :after ((trigger accessor-trigger-volume) slots &key object accessor)
  (let ((obj (object trigger)))
    (when (or object accessor (not (slot-boundp trigger 'getter)))
      (let ((fun (fdefinition (accessor trigger))))
        (setf (getter trigger) (lambda () (funcall fun obj)))))
    (when (or object accessor (not (slot-boundp trigger 'setter)))
      (let ((fun (fdefinition (list 'setf (accessor trigger)))))
        (setf (setter trigger) (lambda (value) (funcall fun value obj)))))))

(defmethod (setf object) (value (trigger accessor-trigger-volume))
  (reinitialize-instance trigger :object value))

(defmethod (setf accessor) (value (trigger accessor-trigger-volume))
  (reinitialize-instance trigger :accessor value))

(defclass kill-trigger-volume (class-filtered-trigger-volume)
  ())

(defmethod activate-trigger ((entity entity) (trigger kill-trigger-volume))
  (leave entity T))

(defclass despawner-trigger-volume (trigger-volume)
  ((spawned-objects :initarg :spawned-objects :initform (tg:make-weak-hash-table :weakness :key) :accessor spawned-objects)))

(defmethod activate-trigger ((entity entity) (trigger despawner-trigger-volume))
  (loop for key being the hash-keys of (spawned-objects trigger)
        do (leave key T)
           (remhash key (spawned-objects trigger))))

(defclass spawner-trigger-volume (trigger-volume)
  ((spawned-objects :initarg :spawned-objects :initform (tg:make-weak-hash-table :weakness :key) :accessor spawned-objects)
   (spawn-class :initarg :spawn-class :accessor spawn-class)
   (spawn-arguments :initarg :spawn-arguments :initform () :accessor spawn-arguments)
   (spawn-count :initarg :spawn-count :initform 1 :accessor spawn-count)
   (spawn-volume :initarg :spawn-volume :initform NIL :accessor spawn-volume)
   (auto-deactivate :initarg :auto-deactivate :initform T :accessor auto-deactivate)
   (respawn-cooldown :initarg :respawn-cooldown :initform NIL :accessor respawn-cooldown)
   (respawn-timer :initform 0 :accessor respawn-timer)))

(define-transfer spawner-trigger-volume spawn-class spawn-arguments spawn-count spawn-volume auto-deactivate respawn-cooldown)

(defun %prune-spawned-objects (spawned-objects)
  (loop for object being the hash-keys of spawned-objects
        do (unless (container object)
             (remhash object spawned-objects)))
  (hash-table-count spawned-objects))

(defmethod draw-instance ((trigger spawner-trigger-volume) &rest args)
  (let ((entity (apply #'draw-instance (spawn-class trigger) (spawn-arguments trigger))))
    (when (spawn-volume trigger)
      (sample-volume (spawn-volume trigger) (location entity)))
    (setf (gethash entity (spawned-objects trigger)) T)
    ;; TODO: make this less horrendously inefficient
    (enter-and-load entity (container trigger) +main+)
    entity))

(defmethod activate-trigger ((entity entity) (trigger spawner-trigger-volume))
  (unless (triggered-p trigger)
    (setf (triggered-p trigger) T)
    (dotimes (i (- (spawn-count trigger) (%prune-spawned-objects (spawned-objects trigger))))
      (draw-instance trigger))))

(define-handler ((trigger spawner-trigger-volume) tick) (dt)
  (when (and (active-p trigger)
             (triggered-p trigger))
    (let ((count (%prune-spawned-objects (spawned-objects trigger))))
      (when (and (= 0 count) (auto-deactivate trigger))
        (setf (active-p trigger) NIL))
      (when (and (< count (spawn-count trigger))
                 (respawn-cooldown trigger))
        (when (<= (decf (respawn-timer trigger) dt) 0.0)
          (setf (respawn-timer trigger) (respawn-cooldown trigger))
          (draw-instance trigger))))))
