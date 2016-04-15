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

(define-override (main key-press-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (issue (scene main) 'key-press
           :key (qt-key->symbol (q+:key ev)))))

(define-override (main key-release-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (issue (scene main) 'key-release
           :key (qt-key->symbol (q+:key ev)))))

(defclass mouse-event (input-event)
  ((pos :initarg :pos :reader pos))
  (:default-initargs
   :pos (error "POS required.")))

(defclass mouse-button-event (input-event)
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
    (format stream "~a => ~a" (old-pos event) (new-pos event))))

(define-override (main mouse-press-event) (ev)
  (issue (scene main) 'mouse-press
         :button (qt-button->symbol (q+:button ev))
         :pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))

(define-override (main mouse-release-event) (ev)
  (issue (scene main) 'mouse-release
         :button (qt-button->symbol (q+:button ev))
         :pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))

(defvar *previous-mouse-position* NIL)
(define-override (main mouse-move-event) (ev)
  (let ((pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))
    (issue (scene main) 'mouse-move
           :old-pos (or *previous-mouse-position* pos)
           :pos pos)
    (setf *previous-mouse-position* pos)))

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

(defun cl-gamepad:device-attached (device)
  (v:info :trial.input "Attached ~s" (cl-gamepad:print-device device NIL))
  (issue (scene *main-window*) 'gamepad-attach
         :device device))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input "Removed ~s" (cl-gamepad:print-device device NIL))
  (issue (scene *main-window*) 'gamepad-remove
         :device device))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (issue (scene *main-window*) 'gamepad-press
         :button (gamepad-button->symbol device button)
         :device device))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (issue (scene *main-window*) 'gamepad-release
         :button (gamepad-button->symbol device button)
         :device device))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (issue (scene *main-window*) 'gamepad-move
         :axis (gamepad-axis->symbol device axis)
         :old-pos last-value
         :pos value
         :device device))
