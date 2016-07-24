#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass input-event (event)
  ())

(defclass keyboard-event (input-event)
  ((key :initarg :key :reader key))
  (:default-initargs
   :key (error "KEY required.")))

(defmethod print-object ((event keyboard-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (key event))))

(defclass key-press (keyboard-event)
  ())

(defclass key-release (keyboard-event)
  ())

(defclass mouse-event (input-event)
  ((pos :initarg :pos :reader pos))
  (:default-initargs
   :pos (error "POS required.")))

(defclass mouse-button-event (mouse-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(defmethod print-object ((event mouse-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (button event))))

(defclass mouse-press (mouse-button-event)
  ())

(defclass mouse-release (mouse-button-event)
  ())

(defclass mouse-move (mouse-event)
  ((old-pos :initarg :old-pos :reader old-pos))
  (:default-initargs
   :old-pos (error "OLD-POS required.")))

(defmethod print-object ((event mouse-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a => ~a" (old-pos event) (pos event))))

(defclass gamepad-event (input-event)
  ((device :initarg :device :reader device))
  (:default-initargs
   :device (error "DEVICE required.")))

(defclass gamepad-attach (gamepad-event)
  ())

(defclass gamepad-remove (gamepad-event)
  ())

(defclass gamepad-press (gamepad-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(defclass gamepad-release (gamepad-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(defclass gamepad-move (gamepad-event)
  ((axis :initarg :axis :reader axis)
   (old-pos :initarg :old-pos :reader old-pos)
   (pos :initarg :pos :reader pos))
  (:default-initargs
   :axis (error "AXIS required.")
   :old-pos (error "OLD-POS required.")
   :pos (error "POS required.")))

(defvar *input-handlers* ())
(defvar *input-handlers-lock* (bt:make-lock))
(defvar *input-gamepad-thread* ())

(define-widget input-handler (QWidget)
  ((previous-pos :initform NIL)))

(define-override (input-handler key-press-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key pressed: ~a" key)
      (handle (make-instance 'key-press :key key) input-handler))))

(define-override (input-handler key-release-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key released: ~a" key)
      (handle (make-instance 'key-release :key key) input-handler))))

(define-override (input-handler mouse-press-event) (ev)
  (let ((button (qt-button->symbol (q+:button ev)))
        (position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))
    (v:debug :trial.input "Mouse pressed: ~a" button)
    (handle (make-instance 'mouse-press :button button :pos position) input-handler)))

(define-override (input-handler mouse-release-event) (ev)
  (let ((button (qt-button->symbol (q+:button ev)))
        (position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))
    (v:debug :trial.input "Mouse released: ~a" button)
    (handle (make-instance 'mouse-release :button button :pos position) input-handler)))

(define-override (input-handler mouse-move-event) (ev)
  (let ((position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))
    (handle (make-instance 'mouse-move :old-pos (or previous-pos position) :pos position) input-handler)
    (setf previous-pos position)))

;; All of this crap is necessary to allow distributing the gamepad events onto
;; multiple instances of input-handlers with potentially multiple targets.
(define-initializer (input-handler register-input-handler)
  (bt:with-lock-held (*input-handlers-lock*)
    (push input-handler *input-handlers*)))

(define-finalizer (input-handler deregister-input-handler)
  (bt:with-lock-held (*input-handlers-lock*)
    (setf *input-handlers* (remove input-handler *input-handlers*))))

(defun init-input-system ()
  (or *input-gamepad-thread*
      (setf *input-gamepad-thread*
            (with-thread ("gamepad event thread")
              (cl-gamepad:init)
              (unwind-protect
                   (loop for i = 0 then (1+ i)
                         while *input-gamepad-thread*
                         do (when (= 0 (mod i 60))
                              (cl-gamepad:detect-devices))
                            (cl-gamepad:process-events)
                            (sleep 1/60))
                (cl-gamepad:shutdown))))))

(defun shutdown-input-system ()
  (with-thread-exit (*input-gamepad-thread*)
    (setf *input-gamepad-thread* NIL)))

(init-input-system)
(pushnew #'shutdown-input-system qtools:*build-hooks*)
(pushnew #'init-input-system qtools:*boot-hooks*)
(pushnew #'shutdown-input-system qtools:*quit-hooks*)

(defun cl-gamepad:device-attached (device)
  (v:info :trial.input "Attached ~s" (cl-gamepad:print-device device NIL))
  (dolist (handler *input-handlers*)
    (handle (make-instance 'gamepad-attach :device device) handler)))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input "Removed ~s" (cl-gamepad:print-device device NIL))
  (dolist (handler *input-handlers*)
    (handle (make-instance 'gamepad-remove :device device) handler)))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (let ((button (gamepad-button->symbol device button)))
    (dolist (handler *input-handlers*)
      (handle (make-instance 'gamepad-press :button button :device device) handler))))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (let ((button (gamepad-button->symbol device button)))
    (dolist (handler *input-handlers*)
      (handle (make-instance 'gamepad-release :button button :device device) handler))))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (let ((axis (gamepad-axis->symbol device axis)))
    (dolist (handler *input-handlers*)
      (handle (make-instance 'gamepad-move :axis axis :old-pos last-value :pos value :device device) handler))))

(define-uniform-retention key (key-press key-release key)
  key)

(define-uniform-retention mouse (mouse-press mouse-release button)
  button)

(define-uniform-retention gamepad (gamepad-press gamepad-release button)
  button)
