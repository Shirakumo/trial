#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass game-object ()
  ((angle :initform 0)
   (angle-delta :initform 1)
   (path :initarg :path :initform NIL :accessor path)
   (image :initform NIL :accessor image))
  (:default-initargs :path (error "Must define a file path.")))

(defmethod initialize-instance :after ((obj game-object) &key)
  (setf (image obj) (load-image-buffer (path obj))))

(defmethod finalize ((obj game-object))
  (finalize (image obj))
  (setf (image obj) NIL)
  (setf (path obj) NIL))

(defmethod update ((obj game-object) &key)
  (incf (slot-value obj 'angle) (slot-value obj 'angle-delta)))

(defmethod draw ((obj game-object) &key)
  (let ((image (image obj)))
    
    (gl:push-matrix)
    (gl:translate 250 250 0)
    (gl:rotate (slot-value obj 'angle) 0 0 1)
    (gl:with-primitives :quads
      (gl:color 1 0 0)
      (gl:vertex -50 -50)
      (gl:color 0 1 0)
      (gl:vertex 50 -50)
      (gl:color 0 0 1)
      (gl:vertex 50 50)
      (gl:color 1 1 1)
      (gl:vertex -50 50))
    (gl:pop-matrix)))
