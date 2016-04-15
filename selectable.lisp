#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *color-id-counter* 0)
(defvar *color-id-map* (trivial-garbage:make-weak-hash-table :test 'eql :weakness :value))

(defun register-object-color (object)
  (let ((id (incf *color-id-counter*)))
    (setf (gethash id *color-id-map*) object)
    id))

(defun color->object (color-id)
  (etypecase color-id
    ((unsigned-byte 32))
    ((vector (unsigned-byte 8) 4)
     (let ((id 0))
       (setf (ldb (byte 8 0) id) (aref color-id 3))
       (setf (ldb (byte 8 8) id) (aref color-id 2))
       (setf (ldb (byte 8 16) id) (aref color-id 1))
       (setf (ldb (byte 8 24) id) (aref color-id 0))
       (setf color-id id))))
  (gethash color-id *color-id-map*))

(define-finalizable selection-buffer (framebuffer)
  ())

(defmethod render (scene (buffer selection-buffer))
  (gl:clear :color-buffer :depth-buffer)
  ;; Disable blending and textures to ensure we have just 32bit colours.
  (gl:disable :blend :texture-2d)
  (gl:enable :depth-test :cull-face :multisample :line-smooth :polygon-smooth)
  (with-pushed-matrix
      (paint scene buffer)))

(defmethod object-at-point ((buffer selection-buffer) x y)
  (with-framebuffer-bound (buffer)
    (color->object (gl:read-pixels x y 1 1 :rgba :unsigned-byte))))

(define-subject selectable-subject ()
  ((color-id :initarg :color-id :accessor color-id))
  (:default-initargs
   :color-id NIL))

(defmethod initialize-instance :after ((subject selectable-subject) &key)
  (unless (color-id subject)
    (setf (color-id subject) (register-object-color subject))))

(defmethod paint :before ((subject selectable-subject) (buffer selection-buffer))
  (gl:color (ldb (byte 8 24) (color-id subject))
            (ldb (byte 8 16) (color-id subject))
            (ldb (byte 8  8) (color-id subject))
            (ldb (byte 8  0) (color-id subject))))
