#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass shader-pass-class (shader-subject-class)
  ((pass-inputs :initarg :pass-inputs :initform () :accessor pass-inputs)))

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass T))
  NIL)

(defmethod c2mop:validate-superclass ((class T) (superclass shader-pass-class))
  NIL)

(defmethod c2mop:validate-superclass ((class shader-pass-class) (superclass standard-class))
  T)

(defclass shader-pass (shader-subject)
  ((pass-inputs :initarg :pass-inputs :initform () :accessor pass-inputs))
  (:metaclass shader-pass-class))

(defgeneric register-object-for-pass (pass object))
(defgeneric shader-program-for-pass (pass object))

(defmacro define-shader-pass (name direct-superclasses inputs &optional slots &rest options)
  (unless (find :metaclass options :key #'car)
    (push '(:metaclass shader-pass-class) options))
  `(defclass ,name (,@direct-superclasses shader-pass)
     ,slots
     ,@options
     (:pass-inputs ,@inputs)))

(define-shader-pass per-object-pass ()
  ()
  ((assets :initform (make-hash-table :test 'eql) :accessor assets)))

(defmethod load progn ((pass per-object-pass))
  (loop for v being the hash-values of (assets pass)
        do (load v)))

(defmethod shader-program-for-pass ((pass per-object-pass) (subject shader-subject))
  (gethash (class-of subject) (assets pass)))

(defmethod coerce-pass-shader ((pass per-object-pass) type spec)
  (glsl-toolkit:merge-shader-sources
   (list (class-shader type pass) spec)))

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
  (let ((program (shader-program-for-pass pass subject)))
    (gl:use-program (resource program))
    ;; FIXME: register inputs as uniforms... ?
    (call-next-method)))

(define-shader-pass single-shader-pass ()
  ()
  ((shader-program :initform (make-instance 'shader-program-asset) :accessor shader-program)))

(defmethod load progn ((pass single-shader-pass))
  (setf (shader-program pass) (make-class-shader-program pass))
  (load (shader-program pass)))

(defmethod offload progn ((pass single-shader-pass))
  (offload (shader-program pass))
  (setf (shader-program pass) NIL))

(defmethod register-object-for-pass ((pass single-shader-pass) o))

(defmethod shader-program-for-pass ((pass single-shader-pass) o)
  (shader-program pass))

(defmethod paint :around ((pass single-shader-pass) target)
  (let ((program (shader-program pass)))
    (gl:use-program (resource program))
    ;; FIXME: register inputs as uniforms... ?
    (call-next-method)))

(define-shader-pass post-effect-pass (single-shader-pass)
  (input)
  ((vertex-array :initform (asset 'geometry 'fullscreen-square) :accessor vertex-array)))

(defmethod load progn ((pass post-effect-pass))
  (load (vertex-array pass)))

(defmethod paint ((pass post-effect-pass) target)
  (let ((vao (vertex-array pass)))
    (gl:disable :depth-test)
    (gl:bind-vertex-array (resource vao))
    (%gl:draw-elements (vertex-form vao) (size vao) :unsigned-int 0)
    (gl:bind-vertex-array 0)
    (gl:enable :depth-test)))

(define-class-shader post-effect-pass :vertex-shader
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 in_texCoord;
out vec2 texCoord;

void main(){
  gl_Position = vec4(position, 1.0f);
  texCoord = in_texCoord;
}")

(define-class-shader post-effect-pass :fragment-shader
  "
in vec2 texCoord;")
