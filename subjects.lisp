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

(defmethod (setf texture) ((asset asset) (subject textured-subject))
  (setf (slot-value subject 'texture) asset))

(defmethod (setf texture) ((null null) (subject textured-subject))
  (setf (slot-value subject 'texture) NIL))

(defmethod (setf texture) (thing (subject textured-subject))
  (setf (texture subject) (asset thing 'texture)))

(defmethod paint :around ((obj textured-subject) target)
  (when (texture obj)
    (bind-texture obj)
    (call-next-method)
    (gl:bind-texture :texture-2d 0)))

(defmethod bind-texture ((obj textured-subject))
  (gl:bind-texture :texture-2d (content (texture obj)))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp))

(defmethod save-form-args append ((subject textured-subject))
  `(:texture ,(texture subject)))

(define-subject located-subject ()
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod paint :around ((obj located-subject) target)
  (with-pushed-matrix
    (let ((location (location obj)))
      (gl:translate (vx location) (vy location) (vz location))
      (call-next-method))))

(defmethod save-form-args append ((subject located-subject))
  `(:location ,(location subject)))

(define-subject oriented-subject ()
  ((orientation :initarg :orientation :accessor orientation)
   (up :initarg :up :accessor up))
  (:default-initargs
   :orientation (vec 1 0 0)
   :up (vec 0 1 0)))

(defmethod paint :around ((obj oriented-subject) target)
  (with-pushed-matrix
    (let ((axis (vc (up obj) (orientation obj)))
          (angle (acos (v. (up obj) (orientation obj)))))
      (gl:rotate angle (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(defmethod save-form-args append ((subject oriented-subject))
  `(:orientation ,(orientation subject)
    :up ,(up subject)))

(define-subject rotated-subject ()
  ((axis :initarg :axis :accessor axis)
   (angle :initarg :angle :accessor angle))
  (:default-initargs
   :axis (vec 0 1 0)
   :angle 0))

(defmethod save-form-args append ((subject rotated-subject))
  `(:axis ,(axis subject)
    :angle ,(angle subject)))

(define-subject mesh-subject ()
  ((mesh :initform NIL :accessor mesh)))

(defmethod initialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod reinitialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod (setf mesh) ((asset asset) (subject mesh-subject))
  (setf (slot-value subject 'mesh) asset))

(defmethod (setf mesh) ((null null) (subject mesh-subject))
  (setf (slot-value subject 'mesh) NIL))

(defmethod (setf mesh) (thing (subject mesh-subject))
  (setf (mesh subject) (asset thing 'model)))

(defmethod paint ((subject mesh-subject) target)
  (loop for mesh across (content (mesh subject))
        do (wavefront-loader:draw mesh)))

(defmethod save-form-args append ((subject mesh-subject))
  `(:mesh ,(mesh subject)))

(define-subject space-axes ()
  ((size :initarg :size :accessor size)
   (grid :initarg :grid :accessor grid))
  (:default-initargs
   :size 10
   :grid 10))

(defmethod paint ((subject space-axes) target)
  (let* ((s (size subject))
         (g (* (/ (grid subject) 2) s)))
    (gl:line-width 1.0)
    (gl:color 0.3 0.3 0.3)
    (with-primitives :lines
      (loop for i from (- g) to g by s
            do (gl:vertex (- g) 0.0 i)
               (gl:vertex g 0.0 i)
               (gl:vertex i 0.0 (- g))
               (gl:vertex i 0.0 g)))
    (gl:line-width 2.0)
    (with-primitives :lines
      (gl:color 1.0 0 0)
      (gl:vertex 0 0 0)
      (gl:vertex s 0 0)
      (gl:color 0 1.0 0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 s 0)
      (gl:color 0 0 1.0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 s)))
  (gl:color 1.0 1.0 1.0))

(defmethod save-form-args append ((subject space-axes))
  `(:size ,(size subject)
    :grid ,(grid subject)))
