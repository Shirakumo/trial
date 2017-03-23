#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass shader-pass-class (standard-class)
  ((pass-inputs :initarg :pass-inputs :initform () :accessor pass-inputs)
   (pass-outputs :initarg :pass-outputs :initform () :accessor pass-outputs)))

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass T))
  NIL)

(defmethod c2mop:validate-superclass ((class T) (superclass shader-pass-class))
  NIL)

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass standard-class))
  T)

(defclass shader-pass ()
  ()
  (:metaclass shader-pass-class))

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-resource-for-pass (pass object))

(defmethod paint ((pass shader-pass) (target main))
  (paint (scene target) pass))

(defmethod pass-inputs ((pass shader-pass))
  (pass-inputs (class-of pass)))

(defmethod pass-outputs ((pass shader-pass))
  (pass-outputs (class-of pass)))

(defmacro define-shader-pass (name direct-superclasses inputs &optional slots &rest options)
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(defclass ,name (shader-pass ,@direct-superclasses)
     ,slots
     ,@options
     (:pass-inputs ,@inputs)))

(define-shader-pass per-object-pass ()
  ()
  ((assets :initform (make-hash-table :test 'eql))))

(defmethod load progn ((pass per-object-pass))
  (loop for v being the hash-values of (assets pass)
        do (load v)))

(defmethod shader-resource-for-pass ((pass per-object-pass) (subject shader-subject))
  (resource (gethash (class-of subject) (assets pass))))

(defmethod coerce-pass-shader ((pass per-object-pass) type spec)
  spec)

(defmethod register-object-for-pass ((pass per-object-pass) (subject shader-subject))
  (let ((shaders ())
        (class (class-of subject)))
    (unless (gethash class (assets pass))
      (loop for (type spec) on (effective-shaders class) by #'cddr
            for inputs = (coerce-pass-shader pass type spec)
            for shader = (make-asset 'shader-asset inputs :type type)
            do (push shader shaders))
      (setf (gethash class (assets pass))
            (make-asset 'shader-program-asset shaders)))))

(defmethod paint :around ((subject shader-subject) (pass per-object-pass))
  (gl:use-program (resource (shader-asset subject)))
  (unwind-protect
       (call-next-method)
    (gl:use-program 0)))

(define-shader-pass single-shader-pass ()
  ()
  ((shader-program :initarg :shader-program :accessor shader-program)))

(defmethod load progn ((pass single-shader-pass))
  (load (shader-program pass)))

(defmethod register-object-for-pass ((pass single-shader-pass) o))
(defmethod shader-resource-for-pass ((pass single-shader-pass) o))

(defmethod paint :around ((pass single-shader-pass) target)
  (gl:use-program (resource (shader-program pass)))
  (unwind-protect
       (call-next-method)
    (gl:use-program 0)))

(define-asset packed-vao-asset (radiance fullscreen-square)
              (#(0 1 2 2 3 0)
                3 #(+1.0 +1.0 +0.0
                    +1.0 -1.0 +0.0
                    -1.0 -1.0 +0.0
                    -1.0 +1.0 +0.0)
                2 #(1.0 1.0
                    1.0 0.0
                    0.0 0.0
                    0.0 1.0)))

(define-shader-pass post-effect-pass (single-shader-pass)
  (input)
  ((vertex-array :initform (asset 'radiance 'fullscreen-square) :accessor vertex-array)))

(defmethod load progn ((pass post-effect-pass))
  (load (vertex-array pass)))

(defmethod paint ((pass post-effect-pass) target)
  (let ((vao (vertex-array pass)))
    (gl:bind-vertex-array (resource vao))
    (%gl:draw-elements (vertex-form subject) (size vao) :unsigned-int 0)
    (gl:bind-vertex-array 0)))
