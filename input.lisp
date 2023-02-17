#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +input-source+ :keyboard)

(defclass input-event (event)
  ())

(defclass keyboard-event (input-event)
  ())

(defclass digital-event (input-event)
  ())

(defgeneric button (digital-event))

(defclass key-event (keyboard-event digital-event)
  ((key :initarg :key :reader key :reader button)
   (repeat :initarg :repeat :initarg :repeat-p :reader repeat-p)
   (modifiers :initarg :modifiers :reader modifiers))
  (:default-initargs
   :key (error "KEY required.")
   :repeat NIL
   :modifiers ()))

(defmethod print-object ((event key-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (key event))))

(defclass key-press (key-event)
  ())

(defclass key-release (key-event)
  ())

(defclass text-entered (keyboard-event)
  ((text :initarg :text :reader text)
   (replace :initarg :replace :initarg :replace-p :initform NIL :reader replace-p))
  (:default-initargs
   :text (error "TEXT required.")))

(defmethod print-object ((event text-entered) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (text event))))

(defclass mouse-event (input-event)
  ((pos :initarg :pos :reader pos))
  (:default-initargs
   :pos (error "POS required.")))

(defclass mouse-button-event (mouse-event digital-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(defmethod print-object ((event mouse-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~s" (button event))))

(defclass mouse-press (mouse-button-event)
  ())

(defclass mouse-release (mouse-button-event)
  ())

(defclass mouse-scroll (mouse-event)
  ((delta :initarg :delta :reader delta))
  (:default-initargs
   :delta (error "DELTA required.")))

(defmethod print-object ((event mouse-scroll) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a" (delta event))))

(defclass mouse-move (mouse-event)
  ((old-pos :initarg :old-pos :reader old-pos))
  (:default-initargs
   :old-pos (error "OLD-POS required.")))

(defmethod print-object ((event mouse-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a => ~a" (old-pos event) (pos event))))

(defclass file-drop-event (mouse-event)
  ((paths :initarg :paths :reader paths)))

(defclass gamepad-event (input-event)
  ((device :initarg :device :reader device))
  (:default-initargs
   :device (error "DEVICE required.")))

(defclass gamepad-button-event (gamepad-event digital-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(defmethod print-object ((event gamepad-button-event) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s" (device event) (button event))))

(defclass gamepad-press (gamepad-button-event)
  ())

(defclass gamepad-release (gamepad-button-event)
  ())

(defclass gamepad-move (gamepad-event)
  ((axis :initarg :axis :reader axis)
   (old-pos :initarg :old-pos :reader old-pos)
   (pos :initarg :pos :reader pos))
  (:default-initargs
   :axis (error "AXIS required.")
   :old-pos (error "OLD-POS required.")
   :pos (error "POS required.")))

(defmethod print-object ((event gamepad-move) stream)
  (print-unreadable-object (event stream :type T)
    (format stream "~a ~s ~3f" (device event) (axis event) (pos event))))

(defclass gamepad-added (gamepad-event)
  ())

(defclass gamepad-removed (gamepad-event)
  ())
