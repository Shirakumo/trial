#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-action ui-action ())

(define-action activate-next (ui-action)
  (mouse-scroll (< 0 delta))
  (key-release (one-of key :down :s))
  (gamepad-release (eql button :dpad-down))
  (gamepad-move (and (eql axis :left-v) (< old-pos -0.5 pos))))

(define-action activate-previous (ui-action)
  (mouse-scroll (< delta 0))
  (key-release (one-of key :up :w))
  (gamepad-release (eql button :dpad-up))
  (gamepad-move (and (eql axis :left-v) (< pos 0.5 old-pos))))

(define-action next-value (ui-action)
  (key-release (one-of key :right :d))
  (gamepad-release (eql button :dpad-right))
  (gamepad-move (and (eql axis :left-h) (< pos 0.5 old-pos))))

(define-action previous-value (ui-action)
  (key-release (one-of key :left :a))
  (gamepad-release (eql button :dpad-left))
  (gamepad-move (and (eql axis :left-h) (< old-pos -0.5 pos))))

(define-action confirm (ui-action)
  (key-release (one-of key :return :enter))
  (gamepad-release (eql button :a)))

(define-action cancel (ui-action)
  (key-release (eql key :escape))
  (gamepad-release (eql button :b)))

(defgeneric point-in-control (point control))

(defmethod point-in-control (point (widget widget))
  (let ((extent (extent widget)))
    (and (<= (vx4 extent) (vx point) (+ (vx4 extent) (vz4 extent)))
         (<= (vy4 extent) (vy point) (+ (vy4 extent) (vw4 extent))))))

(defclass control (entity)
  ((group :initarg :group :accessor group)
   (status :initform :background :accessor status))
  (:default-initargs
   :group NIL))

(defmethod ui-context ((control control))
  (when (group control)
    (ui-context (group control))))

(defmethod handle (event (control control)))

(defmethod handle ((event cancel) (control control))
  (setf (active (ui-context control)) (group control)))

(defmethod handle :around ((event mouse-event) (control control))
  (cond ((point-in-control (pos event) control)
         (unless (eql (status control) :active)
           (setf (status control) :highlighted))
         (call-next-method))
        ((group control)
         (unless (eql (status control) :active)
           (setf (status control) :background))
         (handle event (group control)))))

(defmethod handle ((event mouse-release) (control control))
  (when (eql (button event) :left)
    (setf (active (ui-context control)) control)))

(defclass group (control array-container)
  ((index :initarg :index :accessor index))
  (:default-initargs :index -1))

(defmethod shared-initialize :after ((group group) slots &key (children () c-p))
  (when c-p
    (dolist (child children)
      (enter child group))))

(defmethod enter :after ((control control) (group group))
  (setf (group control) group)
  (unless (<= 0 (index group) (1- (length (objects group))))
    (setf (index group) (1- (length (objects group))))))

(defmethod leave :after ((control control) (group group))
  (setf (parent control) NIL)
  (unless (<= 0 (index group) (1- (length (objects group))))
    (setf (index group) (1- (length (objects group))))))

(defmethod selected ((group group))
  (aref (objects group) (index group)))

(defmethod (setf selected) ((control control) (group group))
  (setf (index group) (or (position control (objects group))
                          (error "~a is not a part of ~a." control group))))

(defmethod (setf index) :before (value (group group))
  (when (< -1 (index group) (length (objects group)))
    (setf (status (aref (objects group) (index group))) :background)))

(defmethod (setf index) :after (value (group group))
  (when (< -1 (index group) (length (objects group)))
    (setf (status (aref (objects group) (index group))) :highlighted)))

(defmethod (setf status) :after ((value (eql :active)) (group group))
  (setf (index group) (index group)))

(defmethod handle ((event confirm) (group group))
  (setf (active (ui-context group)) (selected group)))

(defmethod handle ((event activate-next) (group group))
  (let ((i (index group)))
    (loop do (setf i (mod (1+ i) (length (objects group))))
          until (typep (aref (objects group) i) 'control))
    (setf (index group) i)))

(defmethod handle ((event activate-previous) (group group))
  (let ((i (index group)))
    (loop do (setf i (mod (1- i) (length (objects group))))
          until (typep (aref (objects group) i) 'control))
    (setf (index group) i)))

(defmethod handle ((event mouse-event) (group group))
  (loop for child across (objects group)
        do (when (typep child 'control)
             (if (point-in-control (pos event) child)
                 (handle event child)
                 (setf (status child) :background)))))

(defmethod handle ((event mouse-release) (group group))
  (loop for child across (objects group)
        do (when (and (typep child 'control)
                        (point-in-control (pos event) child))
             (handle event child)
             (return))))

(defclass ui-context (array-container)
  ((active :initform NIL :accessor active)))

(defmethod ui-context ((context ui-context))
  context)

(defmethod enter :after ((control control) (context ui-context))
  (setf (group control) context)
  (unless (active context)
    (setf (active context) control)))

(defmethod leave :after ((control control) (context ui-context))
  (setf (group control) NIL)
  (when (eql control (active context))
    (setf (active context) NIL)))

(defmethod (setf active) :before (control (context ui-context))
  (when (active context)
    (setf (status (active context)) :background)))

(defmethod (setf active) :after ((control control) (context ui-context))
  (setf (status control) :active))

(defmethod (setf active) ((new null) (context ui-context))
  (if (= 0 (length (objects context)))
      (call-next-method)
      (setf (active context) (aref (objects context) 0))))

(defmethod (setf active) :around ((new ui-context) (context ui-context))
  context)

(defmethod handle (event (context ui-context))
  (when (active context)
    (handle event (active context))))
