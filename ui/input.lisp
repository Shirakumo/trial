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
  (key-release (eql key :return))
  (gamepad-release (eql button :a)))

(define-action cancel (ui-action)
  (key-release (eql key :escape))
  (gamepad-release (eql button :b)))

(defgeneric point-in-control (point control))

(defmethod point-in-control (point (ui-element ui-element))
  (and (<= (vx4 ui-element) (vx point) (+ (vx4 ui-element) (vz4 ui-element)))
       (<= (vy4 ui-element) (vy point) (+ (vy4 ui-element) (vw4 ui-element)))))

(defclass control ()
  ((group :initarg :group :accessor group)
   (status :initform :background :accessor status)))

(defmethod ui-context ((control control))
  (ui-context (group control)))

(defmethod handle ((event cancel) (control control))
  (setf (status control) :background)
  (setf (active (ui-context control)) (group control)))

(defmethod handle ((event mouse-event) (control control))
  (cond ((point-in-control (pos event) control)
         (setf (status control) :highlighted))
        ((eq control (active (ui-context control)))
         (when (group control)
           (handle event (group control))))))

(defmethod handle ((event mouse-release) (control control))
  (cond ((point-in-control (pos event) control)
         (when (eql (button event) :left)
           (setf (active (ui-context control)) control)))
        ((eq control (active (ui-context control)))
         (when (group control)
           (handle event (group control))))))

(defclass group (control)
  ((children :initform #() :accessor children)
   (index :initarg :index :accessor index))
  (:default-initargs :index -1))

(defmethod selected ((group group))
  (aref (children group) (index group)))

(defmethod (setf selected) ((control control) (group group))
  (setf (index group) (or (position control (children group))
                          (error "~a is not a part of ~a." control group))))

(defmethod (setf index) :before (value (group group))
  (when (< -1 (index group) (length (children group)))
    (setf (status (aref (children group) (index group))) NIL)))

(defmethod (setf index) :after (value (group group))
  (when (< -1 (index group) (length (children group)))
    (setf (status (aref (children group) (index group))) :highlighted)))

(defmethod handle ((event confirm) (group group))
  (setf (active (ui-context group)) (selected group)))

(defmethod handle ((event activate-next) (group group))
  (setf (index group) (mod (1+ (index group)) (length (children group)))))

(defmethod handle ((event activate-previous) (group group))
  (setf (index group) (mod (1- (index group)) (length (children group)))))

(defmethod handle ((event mouse-event) (group group))
  (cond ((point-in-control (pos event) control)
         (loop with ui-context = (ui-context group)
               for child across (children group)
               unless (eql child (active ui-context))
               do (handle event child)
               until (eql child (active ui-context))))
        ((eq control (active (ui-context control)))
         (when (group control)
           (handle event (group control))))))

(defclass ui-context (group)
  ((active :initform NIL :accessor active)))

(defmethod ui-context ((context ui-context))
  context)

(defmethod (setf active) :before (control (context ui-context))
  (when (active context)
    (setf (status (active context)) NIL)))

(defmethod (setf active) :after ((control control) (context ui-context))
  (setf (status control) :active))

(defmethod handle (event (context ui-context))
  (handle event (active context)))
