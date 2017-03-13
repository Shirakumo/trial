#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-subject-class (subject-class)
  ((effective-shaders :initform () :accessor effective-shaders)
   (direct-shaders :initform () :accessor direct-shaders)
   (shader-asset :initform NIL :accessor shader-asset)
   (dirty :initform T :accessor dirty)))

(defmethod cascade-option-changes :before ((class shader-subject-class))
  (let ((shaders-list (direct-shaders class))
        (effective-shaders ()))
    (loop for super in (c2mop:class-direct-superclasses class)
          do (when (typep super 'shader-subject-class)
               (loop for (type shader) on (shaders-list super) by #'cddr
                     do (pushnew shader (getf effective-shaders type)))))
    (loop for (type shaders) on effective-shaders by #'cddr
          do (setf (getf effective-shaders type)
                   (glsl-toolkit:merge-shader-sources
                    (nreverse shaders))))
    (setf (effective-shaders class) effective-shaders)
    (setf (dirty class) T)))

(defmethod apply-uniforms ((class shader-subject-class))
  )

(defclass shader-subject (subject)
  ()
  (:metaclass shader-subject-class))

(defmethod shader-asset ((subject shader-subject))
  (shader-asset (class-of subject)))

(defmethod reinitialize-instance :after ((subject shader-subject) &key)
  (let ((class (class-of subject)))
    (when (and *context* (dirty class))
      (when (shader-asset class)
        (offload-asset (shader-asset class)))
      (let ((shaders ()))
        (loop for (type spec) on (effective-shaders class) by #'cddr
              for shader = (make-asset 'shader-asset spec :type type)
              do (load-asset shader)
                 (push shader shaders))
        (setf (shader-asset class) (make-asset 'shader-program shaders)))
      (load-asset (shader-asset class)))))

(defmethod paint :around ((subject shader-subject) target)
  (gl:use-program (resource (shader-asset subject)))
  (unwind-protect
       (call-next-method)
    (gl:use-program 0)))

(defmethod paint :before ((subject shader-subject) target)
  (apply-uniforms (class-of subject)))
