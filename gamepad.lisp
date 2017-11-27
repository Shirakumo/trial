#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gamepad-handlers* ())
(defvar *gamepad-handlers-lock* (bt:make-lock))
(defvar *gamepad-input-thread* ())

(defun add-gamepad-handler (handler)
  (bt:with-lock-held (*gamepad-handlers-lock*)
    (pushnew handler *gamepad-handlers*)))

(defun remove-gamepad-handler (handler)
  (bt:with-lock-held (*gamepad-handlers-lock*)
    (setf *gamepad-handlers* (remove handler *gamepad-handlers*))))

(defun init-gamepad-system ()
  (or *gamepad-input-thread*
      (setf *gamepad-input-thread*
            (with-thread ("gamepad event thread")
              (cl-gamepad:init)
              (unwind-protect
                   (loop for i = 0 then (1+ i)
                         while *gamepad-input-thread*
                         do (when (= 0 (mod i 60))
                              (cl-gamepad:detect-devices))
                            (cl-gamepad:process-events)
                            (sleep 1/60))
                (cl-gamepad:shutdown))))))

(defun shutdown-gamepad-system ()
  (with-thread-exit (*gamepad-input-thread*)
    (setf *gamepad-input-thread* NIL)))

(init-gamepad-system)

(defun cl-gamepad:device-attached (device)
  (v:info :trial.input "Attached ~s (~:[Unknown~;~:*~a~])"
          (cl-gamepad:print-device device NIL)
          (gamepad-info device))
  (dolist (handler *gamepad-handlers*)
    (handle (make-instance 'gamepad-attach :device device) handler)))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input "Removed ~s" (cl-gamepad:print-device device NIL))
  (dolist (handler *gamepad-handlers*)
    (handle (make-instance 'gamepad-remove :device device) handler)))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (let ((button (gethash button (gamepad-info device))))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-press :button button :device device) handler))))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (let ((button (gethash button (gamepad-info device))))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-release :button button :device device) handler))))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (destructuring-bind (axis modifier) (gethash axis (gamepad-info device))
    (dolist (handler *gamepad-handlers*)
      (handle (make-instance 'gamepad-move :axis axis :old-pos (* modifier last-value) :pos  (* modifier value) :device device) handler))))
