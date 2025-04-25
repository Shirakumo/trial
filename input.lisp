(in-package #:org.shirakumo.fraf.trial)

(define-global +default-input-source+
  #-nx :keyboard #+nx org.shirakumo.fraf.gamepad.impl::*default-device*)
(define-global +input-source+
  +default-input-source+)

(define-event input-event (event))
(define-event keyboard-event (input-event))
(define-event digital-event (input-event))
(define-event press-event (input-event))
(define-event release-event (input-event))

(defgeneric button (digital-event))

(define-event key-event (keyboard-event digital-event)
  (key arg! :reader key :reader button) (repeat NIL :reader repeat-p) (modifiers ()))

(defmethod print-object ((event key-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (key event))))

(define-event key-press (key-event press-event) :pool T)
(define-event key-release (key-event release-event) :pool T)

(define-event text-entered (keyboard-event) :pool T
  text (replace NIL :reader replace-p))

(defmethod print-object ((event text-entered) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (text event))))

(define-event mouse-event (input-event)
  pos)

(define-event mouse-button-event (mouse-event digital-event)
  button)

(defmethod print-object ((event mouse-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (button event))))

(define-event mouse-press (mouse-button-event press-event) :pool T)
(define-event mouse-release (mouse-button-event release-event) :pool T)
(define-event mouse-double-click (mouse-button-event) :pool T)
(define-event mouse-scroll (mouse-event) :pool T
  delta)

(defmethod print-object ((event mouse-scroll) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (delta event))))

(define-event mouse-move (mouse-event) :pool 128
  old-pos)

(defmethod print-object ((event mouse-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a => ~a" (old-pos event) (pos event))))

(define-event file-drop-event (mouse-event) :pool T
  paths)

(define-event gamepad-event (input-event)
  device)

(defmethod print-object ((event gamepad-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (device event))))

(define-event gamepad-button-event (gamepad-event digital-event)
  button)

(defmethod print-object ((event gamepad-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s" (device event) (button event))))

(define-event gamepad-press (gamepad-button-event press-event) :pool T)
(define-event gamepad-release (gamepad-button-event release-event) :pool T)

(define-event gamepad-move (gamepad-event) :pool T
  axis old-pos pos)

(defmethod print-object ((event gamepad-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s ~3f" (device event) (axis event) (pos event))))

(define-event gamepad-added (gamepad-event) :pool T)
(define-event gamepad-removed (gamepad-event) :pool T)
