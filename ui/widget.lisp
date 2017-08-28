#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(defclass widget (entity)
  ((parent :initform NIL :initarg :parent :accessor parent)
   (extent :initform (vec4 0 0 0 0) :initarg :extent :accessor extent)
   (preferred-size :initform (vec2 1 1) :initarg :preferred-size :accessor preferred-size)
   (visible-p :initform T :initarg :visible-p :accessor visible-p)))

(defmethod (setf visible-p) :after ((visibility null) (widget widget))
  (note-extent-change widget widget))

(defmethod (setf extent) :after (extent (widget widget))
  (note-extent-change widget widget))

(defmethod width ((widget widget))
  (vz4 (extent widget)))

(defmethod (setf width) (value (widget widget))
  (prog1 (setf (vz4 (extent widget)) value)
    (note-extent-change widget widget)))

(defmethod height ((widget widget))
  (vw4 (extent widget)))

(defmethod (setf height) (value (widget widget))
  (prog1 (setf (vw4 (extent widget)) value)
    (note-extent-change widget widget)))

(defmethod preferred-width ((widget widget))
  (vx2 (preferred-size widget)))

(defmethod (setf preferred-width) (value (widget widget))
  (prog1 (setf (vx2 (preferred-size widget)) value)
    (note-extent-change widget widget)))

(defmethod preferred-height ((widget widget))
  (vy2 (preferred-size widget)))

(defmethod (setf preferred-height) (value (widget widget))
  (prog1 (setf (vy2 (preferred-size widget)) value)
    (note-extent-change widget widget)))

(defmethod note-extent-change ((widget widget) (other widget))
  (when (parent widget)
    (note-extent-change (parent widget) widget)))

