#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gamepad-handler*)

(defclass gamepad-input-handler ()
  ((last-device-probe :initform 0 :accessor last-device-probe)))

(defmethod start :after ((handler gamepad-input-handler))
  (let ((*gamepad-handler* handler))
    (cl-gamepad:init)))

(defmethod stop :after ((handler gamepad-input-handler))
  (let ((*gamepad-handler* handler))
    (cl-gamepad:shutdown)))

(defmethod poll-input :after ((handler gamepad-input-handler))
  (let ((*gamepad-handler* handler))
    (cl-gamepad:process-events)
    (when (< internal-time-units-per-second
             (- (get-internal-real-time) (last-device-probe handler)))
      (setf (last-device-probe handler) (get-internal-real-time))
      (cl-gamepad:detect-devices))))

(setf (v:repl-level) :trace)
(defun cl-gamepad:device-attached (device)
  (v:info :trial.input.gamepad "Attached ~s"
          (cl-gamepad:print-device device NIL))
  (handle (make-instance 'gamepad-attach :device device)
          *gamepad-handler*))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input.gamepad "Removed ~s" (cl-gamepad:print-device device NIL))
  (handle (make-instance 'gamepad-remove :device device)
          *gamepad-handler*))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (let ((button (cl-gamepad:button-label device button)))
    (v:trace :trial.input.gamepad "~a pressed  ~a" (cl-gamepad:id device) button)
    (handle (make-instance 'gamepad-press :button button :device device)
            *gamepad-handler*)))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (let ((button (cl-gamepad:button-label device button)))
    (v:trace :trial.input.gamepad "~a released ~a" (cl-gamepad:id device) button)
    (handle (make-instance 'gamepad-release :button button :device device)
            *gamepad-handler*)))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (let ((axis (cl-gamepad:axis-label device axis))
        (mult (cl-gamepad:axis-multiplier device axis)))
    (handle (make-instance 'gamepad-move :axis axis :old-pos (* mult last-value) :pos (* mult value) :device device)
            *gamepad-handler*)))
