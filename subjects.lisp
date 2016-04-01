#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject textured-subject ()
  ((texture :initform NIL :accessor texture :finalized T)))

(defmethod initialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod reinitialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod (setf texture) :around (texture (subject textured-subject))
  (let ((prev (finalize (texture subject))))
    (call-next-method)
    (finalize prev)))

(defmethod (setf texture) (texture (subject textured-subject))
  (setf (slot-value subject 'texture)
        (qtypecase texture
          (QImage (image->framebuffer texture))
          (QGLFramebufferObject texture)
          (T (error "Don't know how to use ~a as a texture for ~a." texture subject)))))

(defmethod (setf texture) ((path pathname) (subject textured-subject))
  (setf (texture subject) (load-image-buffer (resource-pathname path))))

(defmethod (setf texture) ((path string) (subject textured-subject))
  (setf (texture subject) (uiop:parse-native-namestring path)))

(defmethod (setf texture) ((null null) (subject textured-subject))
  (setf (slot-value subject 'texture) NIL))

(defmethod draw :around ((obj textured-subject))
  (when (texture obj)
    (call-next-method)))

(defmethod bind-texture ((obj textured-subject))
  (gl:bind-texture :texture-2d (q+:texture (texture obj)))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp))

(define-subject located-subject ()
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod draw :around ((obj located-subject))
  (gl:with-pushed-matrix
    (let ((location (location obj)))
      (gl:translate (vx location) (vy location) (vz location))
      (call-next-method))))

(define-subject oriented-subject ()
  ((orientation :initarg :orientation :accessor orientation)
   (up :initarg :up :accessor up))
  (:default-initargs
   :orientation (vec 1 0 0)
   :up (vec 0 1 0)))

(defmethod draw :around ((obj oriented-subject))
  (gl:with-pushed-matrix
    (let ((axis (vc (up obj) (orientation obj)))
          (angle (acos (v. (up obj) (orientation obj)))))
      (gl:rotate angle (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(define-subject rotated-subject ()
  ((axis :initarg :axis :accessor axis)
   (angle :initarg :angle :accessor angle))
  (:default-initargs
   :axis (vec 0 1 0)
   :angle 0))
