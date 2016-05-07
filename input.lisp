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

(define-override (display key-press-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key pressed: ~a" key)
      (issue (scene display) 'key-press :key key))))

(define-override (display key-release-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key release: ~a" key)
      (issue (scene display) 'key-release :key key))))

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

(define-override (display mouse-press-event) (ev)
  (issue (scene display) 'mouse-press
         :button (qt-button->symbol (q+:button ev))
         :pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))

(define-override (display mouse-release-event) (ev)
  (issue (scene display) 'mouse-release
         :button (qt-button->symbol (q+:button ev))
         :pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))

(defvar *previous-mouse-position* NIL)
(define-override (display mouse-move-event) (ev)
  (let ((pos (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)) 0)))
    (issue (scene display) 'mouse-move
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

;; FIXME: Currently only the *MAIN* window can ever receive
;;        gamepad input events. This might be fine in most
;;        cases, but sometimes one might also want to receive
;;        the events in another window. What if you have, say,
;;        an input debugging dialogue or something like that?

(defun cl-gamepad:device-attached (device)
  (v:info :trial.input "Attached ~s" (cl-gamepad:print-device device NIL))
  (issue (scene *main*) 'gamepad-attach
         :device device))

(defun cl-gamepad:device-removed (device)
  (v:info :trial.input "Removed ~s" (cl-gamepad:print-device device NIL))
  (issue (scene *main*) 'gamepad-remove
         :device device))

(defun cl-gamepad:button-pressed (button time device)
  (declare (ignore time))
  (issue (scene *main*) 'gamepad-press
         :button (gamepad-button->symbol device button)
         :device device))

(defun cl-gamepad:button-released (button time device)
  (declare (ignore time))
  (issue (scene *main*) 'gamepad-release
         :button (gamepad-button->symbol device button)
         :device device))

(defun cl-gamepad:axis-moved (axis last-value value time device)
  (declare (ignore time))
  (issue (scene *main*) 'gamepad-move
         :axis (gamepad-axis->symbol device axis)
         :old-pos last-value
         :pos value
         :device device))

