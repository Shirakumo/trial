(in-package #:org.shirakumo.fraf.trial)

(defclass trigger-volume (rigid-shape basic-node)
  ((active-p :initarg :active-p :initform T :accessor active-p)
   (triggered-p :initform NIL :accessor triggered-p)))

(define-transfer trigger-volume active-p triggered-p)

(defgeneric activate-trigger (target trigger))

(defmethod awake-p ((entity trigger-volume))
  NIL)

(defmethod collides-p ((a ray) (b trigger-volume) hit)
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

(defclass type-filtered-trigger-volume (trigger-volume)
  ((type-expression :initarg :type-expression :initform 'rigid-shape :accessor type-expression)))

(define-transfer type-filtered-trigger-volume type-expression)

(defmethod collides-p ((a rigid-shape) (trigger type-filtered-trigger-volume) hit)
  (and (typep a (type-expression trigger)) (call-next-method)))

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
  (ecase (action trigger)
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

(defclass kill-trigger-volume (type-filtered-trigger-volume)
  ())

(defmethod activate-trigger ((entity entity) (trigger kill-trigger-volume))
  (leave entity T))

(defclass despawner-trigger-volume (trigger-volume)
  ((spawned-objects :initarg :spawned-objects :initform (tg:make-weak-hash-table :weakness :key) :accessor spawned-objects)))

(defmethod activate-trigger ((entity entity) (trigger despawner-trigger-volume))
  (loop for key being the hash-keys of (spawned-objects trigger)
        do (leave key T)
           (remhash key (spawned-objects trigger))))

(defclass spawner-trigger-volume (type-filtered-trigger-volume listener)
  ((spawned-objects :initarg :spawned-objects :initform (tg:make-weak-hash-table :weakness :key) :accessor spawned-objects)
   (spawn-class :initarg :spawn-class :accessor spawn-class)
   (spawn-arguments :initarg :spawn-arguments :initform () :accessor spawn-arguments)
   (spawn-count :initarg :spawn-count :initform 1 :accessor spawn-count)
   (spawn-volume :initarg :spawn-volume :initform NIL :accessor spawn-volume)
   (spawn-orientation :initarg :spawn-orientation :initform (load-time-value (cons (vec 0 0 0) (vec 0 F-2PI 0))) :accessor spawn-orientation)
   (snap-to-level-p :initarg :snap-to-level :initform T :accessor snap-to-level-p)
   (auto-deactivate :initarg :auto-deactivate :initform T :accessor auto-deactivate)
   (respawn-cooldown :initarg :respawn-cooldown :initform NIL :accessor respawn-cooldown)
   (respawn-timer :initform 0 :accessor respawn-timer)))

(define-transfer spawner-trigger-volume spawn-class spawn-arguments spawn-count spawn-volume auto-deactivate respawn-cooldown)

;; min and max euler angles
(defun evaluate-orientation (min max &optional (q (quat)))
  (declare (type vec3 min max))
  (let ((x (qfrom-angle +vx+ (random* (vx min) (vx max))))
        (y (qfrom-angle +vy+ (random* (vy min) (vy max))))
        (z (qfrom-angle +vz+ (random* (vz min) (vz max)))))
    (declare (dynamic-extent x y z))
    (!q* q (!q* q x y) z)))

(defun %prune-spawned-objects (spawned-objects)
  (loop for object being the hash-keys of spawned-objects
        do (unless (container object)
             (remhash object spawned-objects)))
  (hash-table-count spawned-objects))

(defmethod draw-instance ((trigger spawner-trigger-volume) &rest args)
  (let ((entity (apply #'draw-instance (spawn-class trigger) (append args (spawn-arguments trigger)))))
    (setf (gethash entity (spawned-objects trigger)) T)
    (setf (name entity) NIL)
    ;; TODO: make this less horrendously inefficient
    (enter-and-load entity (container trigger) +main+)
    ;; ENTER-AND-LOAD can reset the object's transform, so we only change it now.
    (if (spawn-volume trigger)
        (sample-volume (spawn-volume trigger) (location entity))
        (v<- (location entity) (location trigger)))
    (destructuring-bind (min . max) (spawn-orientation trigger)
      (evaluate-orientation min max (orientation entity)))
    (when (snap-to-level-p trigger)
      (snap-object-to-level entity))
    entity))

(defmethod activate-trigger ((entity entity) (trigger spawner-trigger-volume))
  (unless (triggered-p trigger)
    ;; We only draw one per activation to avoid stacking
    (if (< (%prune-spawned-objects (spawned-objects trigger)) (spawn-count trigger))
        (draw-instance trigger)
        (setf (triggered-p trigger) T))))

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

(defclass simple-trigger-volume (one-shot-trigger-volume type-filtered-trigger-volume thunk-trigger-volume)
  ())

(defclass checked-trigger-volume (trigger-volume)
  ((condition :accessor condition-fun :initform (constantly NIL) :initarg :condition)))

(define-transfer checked-trigger-volume condition-fun)

(defmethod shared-initialize :after ((trigger checked-trigger-volume) slots &key condition)
  (etypecase condition
    (null)
    (function
     (setf (condition-fun trigger) condition))
    (cons
     (setf (condition-fun trigger) (compile NIL `(lambda () ,condition))))))

(defmethod trigger-passes-p ((trigger checked-trigger-volume))
  (funcall (condition-fun trigger)))

(defmethod collides-p :around ((a rigidbody) (trigger checked-trigger-volume) hit)
  (when (trigger-passes-p trigger)
    (call-next-method)))

(define-global +global-sequences+ (make-hash-table :test 'eql))

(define-event global-sequence-changed () sequence-id old-value value)

(defun global-sequence-value (sequence)
  (gethash sequence +global-sequences+))

(defun (setf global-sequence-value) (value sequence &key (trigger :on-change))
  (let ((old-value (gethash sequence +global-sequences+)))
    (when (ecase trigger
            ((T) T)
            ((NIL) NIL)
            (:on-change (eql value old-value)))
      (issue T 'global-sequence-changed :sequence-id sequence :old-value old-value :value value)))
  (setf (gethash sequence +global-sequences+) value)
  value)

(defclass global-sequence-trigger (one-shot-trigger-volume checked-trigger-volume type-filtered-trigger-volume)
  ((sequence-id :initform T :initarg :sequence-id :accessor sequence-id)
   (modulation :initform #'+ :initarg :modulation :accessor modulation)
   (new-value :initform 1 :initarg :new-value :accessor new-value)))

(define-transfer global-sequence-trigger sequence-id modulation new-value)

(defmethod value ((trigger global-sequence-trigger))
  (global-sequence-value (sequence-id trigger)))

(defmethod activate-trigger (a (trigger global-sequence-trigger))
  (setf (global-sequence-value (sequence-id trigger))
        (funcall (modulation trigger) (value trigger) (new-value trigger))))

(defclass checkpoint-trigger (one-shot-trigger-volume type-filtered-trigger-volume)
  ((spawn-point :initform (vec 0 0 0) :initarg :spawn-point :accessor spawn-point)))

(define-transfer checkpoint-trigger spawn-point)

(defmethod activate-trigger (a (trigger checkpoint-trigger)))

(defclass audio-trigger (type-filtered-trigger-volume)
  ((audio :initarg :audio :accessor audio)))

(define-transfer audio-trigger audio)

(defmethod stage :after ((trigger audio-trigger) (area staging-area))
  (stage (audio trigger) area))

(defmethod activate-trigger (thing (trigger audio-trigger))
  (play (audio trigger) thing))
