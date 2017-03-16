#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-subject-class (subject-class)
  ((effective-shaders :initform () :accessor effective-shaders)
   (direct-shaders :initform () :initarg :shaders :accessor direct-shaders)
   (shader-asset :initform NIL :accessor shader-asset)
   (dirty :initform T :accessor dirty)))

(defmethod cascade-option-changes :before ((class shader-subject-class))
  (let ((effective-shaders ()))
    (loop for (type shader) on (direct-shaders class) by #'cddr
          do (setf (getf effective-shaders type)
                   (list shader)))
    (loop for super in (c2mop:class-direct-superclasses class)
          do (when (typep super 'shader-subject-class)
               (loop for (type shader) on (effective-shaders super) by #'cddr
                     do (pushnew shader (getf effective-shaders type)))))
    (loop for (type shaders) on effective-shaders by #'cddr
          do (setf (getf effective-shaders type)
                   (glsl-toolkit:merge-shader-sources
                    (nreverse shaders))))
    (setf (effective-shaders class) effective-shaders)
    (setf (dirty class) T)))

(defmethod class-shader (type (class shader-subject-class))
  (getf (direct-shaders class) type))

(defmethod class-shader (type (class symbol))
  (class-shader type (find-class class)))

(defmethod (setf class-shader) (shader type (class shader-subject-class))
  (setf (getf (direct-shaders class) type) shader))

(defmethod (setf class-shader) (shader type (class symbol))
  (setf (class-shader type (find-class class)) shader))

(defmethod (setf class-shader) :after (shader type (class shader-subject-class))
  (cascade-option-changes class))

(defmethod remove-class-shader (type (class shader-subject-class))
  (remf (direct-shaders class) type))

(defmethod remove-class-shader (type (class symbol))
  (remove-class-shader type (find-class class)))

(defmethod remove-class-shader :after (type (class shader-subject-class))
  (cascade-option-changes class))

(defmacro define-class-shader (class type &body definitions)
  `(setf (class-shader ,type ',class)
         (progn ,@definitions)))

(defmethod (setf uniform) (data (class shader-subject-class) name)
  (setf (uniform (shader-asset class) name) data))

(defclass shader-subject (subject)
  ()
  (:metaclass shader-subject-class))

(defmethod shader-asset ((subject shader-subject))
  (shader-asset (class-of subject)))

(defmethod (setf uniform) (data (subject shader-subject) name)
  (setf (uniform (shader-asset (class-of subject)) name) data))

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

(defmacro define-shader-subject (&environment env name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp (find-class c T env) 'shader-subject)) direct-superclasses)
    (setf direct-superclasses (append direct-superclasses (list 'shader-subject))))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass shader-subject-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))
