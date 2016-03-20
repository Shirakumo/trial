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
  (let ((image (q+:texture (image obj)))
        (size (q+:size (image obj))))
    (gl:push-matrix)
    (gl:translate 250 250 0)
    (gl:rotate (slot-value obj 'angle) 0 0 1)
    (q+:draw-texture *main-window*
                     (q+:make-qpointf (- (/ (q+:width size) 2))
                                      (- (/ (q+:height size) 2)))
                     image)
    (gl:pop-matrix)))
