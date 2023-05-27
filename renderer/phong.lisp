#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (phong-material (:include material))
  (textures NIL :initform (vector (// 'trial 'missing) (// 'trial 'black) (// 'trial 'neutral-normal)))
  (diffuse-factor :vec4 :initarg :diffuse-factor :initform (vec 1 1 1 1) :accessor diffuse-factor)
  (specular-factor :float :initarg :specular-factor :initform 1.0 :accessor specular-factor)
  (alpha-cutoff :float :initarg :alpha-cutoff :initform 0.5 :accessor alpha-cutoff))

(defmethod diffuse-texture ((material phong-material))
  (aref (textures material) 0))

(defmethod specular-texture ((material phong-material))
  (aref (textures material) 1))

(defmethod normal-texture ((material phong-material))
  (aref (textures material) 2))

(define-gl-struct phong-material-block
  (size NIL :initarg :size :initform 64 :reader size)
  (materials (:array (:struct phong-material) size) :accessor materials))

(defmethod update-material ((block phong-material-block) (material phong-material) id)
  (let ((target (svref (materials block) id)))
    (setf (diffuse-factor target) (diffuse-factor material))
    (setf (specular-factor target) (specular-factor material))
    (setf (alpha-cutoff target) (alpha-cutoff material))))

(define-shader-pass phong-render-pass (standard-render-pass)
  ()
  (:shader-file (trial "standard-render-phong.glsl")))

(defmethod render-with :before ((pass phong-render-pass) (object standard-renderable) program)
  (setf (uniform program "material_id") (local-id (material object) pass)))

(defmethod render-with ((pass standard-render-pass) (material phong-material) program)
  (setf (uniform program "diffuse_tex") (local-id (diffuse-texture material) pass))
  (setf (uniform program "specular_tex") (local-id (specular-texture material) pass))
  (setf (uniform program "normal_tex") (local-id (normal-texture material) pass)))

(defmethod material-block-type ((pass phong-render-pass))
  'phong-material-block)

(defmethod coerce-object ((material pbr-material) (type (eql 'phong-material)) &key)
  (make-instance 'phong-material
                 :diffuse-texture (albedo-texture material)
                 :specular-texture (metal-rough-occlusion-texture material)
                 :normal-texture (normal-texture material)
                 :diffuse-factor (diffuse-factor material)
                 :specular-factor (vx (metallic-factor material))
                 :alpha-cutoff (alpha-cutoff material)))
