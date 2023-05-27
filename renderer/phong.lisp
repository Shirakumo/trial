#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (phong-material (:include material))
  (diffuse-texture NIL :initarg :diffuse-texture :accessor diffuse-texture)
  (specular-texture NIL :initarg :specular-texture :accessor specular-texture)
  (normal-texture NIL :initarg :normal-texture :accessor normal-texture)
  (diffuse-factor :vec4 :initarg :diffuse-factor :initform (vec 1 1 1 1) :accessor diffuse-factor)
  (specular-factor :vec3 :initarg :specular-factor :initform (vec 1 1 1) :accessor specular-factor)
  (alpha-cutoff :float :initarg :alpha-cutoff :initform 0.5 :accessor alpha-cutoff))

(define-gl-struct phong-material-block
  (size NIL :initarg :size :initform 64 :reader size)
  (material-count :int)
  (materials (:array (:struct phong-material) size)))

(define-shader-pass phong-render-pass (standard-render-pass)
  ()
  (:shader-file (trial "standard-render-phong.glsl")))

(defmethod render-with :before ((pass phong-render-pass) (object standard-renderable) program)
  (setf (uniform program "material_id") (local-id (material object) pass)))

(defmethod prepare-pass-program ((material phong-material) program)
  (setf (uniform program "diffuse_tex") (diffuse-texture material))
  (setf (uniform program "specular_tex") (specular-texture material))
  (setf (uniform program "normal_tex") (normal-texture material)))

(defmethod material-block-type ((pass phong-render-pass))
  'phong-material-block)
