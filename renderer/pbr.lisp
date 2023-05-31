#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (pbr-material (:include material))
  (textures NIL :initform (vector (// 'trial 'missing) (// 'trial 'black) (// 'trial 'black) (// 'trial 'neutral-normal)))
  (albedo-factor :vec4 :initform (vec 1 1 1 1) :initarg :albedo-factor :accessor albedo-factor)
  (emission-factor :vec3 :initform (vec 1 1 1) :initarg :emission-factor :accessor emission-factor)
  (metallic-factor :float :initform 1.0 :initarg :metallic-factor :accessor metallic-factor)
  (roughness-factor :float :initform 1.0 :initarg :roughness-factor :accessor roughness-factor)
  (occlusion-factor :float :initform 1.0 :initarg :occlusion-factor :accessor occlusion-factor)
  (alpha-cutoff :float :initform 0.5 :initarg :alpha-cutoff :accessor alpha-cutoff))

(defmethod albedo-texture ((material pbr-material))
  (aref (textures material) 0))

(defmethod metal-rough-occlusion-texture ((material pbr-material))
  (aref (textures material) 1))

(defmethod emission-texture ((material pbr-material))
  (aref (textures material) 2))

(defmethod normal-texture ((material pbr-material))
  (aref (textures material) 3))

(define-gl-struct pbr-material-block
  (size NIL :initarg :size :initform 64 :reader size)
  (materials (:array (:struct pbr-material) size) :accessor materials))

(defmethod transfer-to progn ((target pbr-material) (material pbr-material))
  (setf (albedo-factor target) (albedo-factor material))
  (setf (emission-factor target) (emission-factor material))
  (setf (metallic-factor target) (metallic-factor material))
  (setf (roughness-factor target) (roughness-factor material))
  (setf (occlusion-factor target) (occlusion-factor material))
  (setf (alpha-cutoff target) (alpha-cutoff material)))

(define-shader-pass pbr-render-pass (standard-shadows-pass light-cache-render-pass)
  ()
  (:shader-file (trial "standard-render-pbr.glsl")))

(defmethod render-with ((pass pbr-render-pass) (material pbr-material) program)
  (enable material pass)
  (setf (uniform program "material_id") (local-id material pass))
  (setf (uniform program "albedo_tex") (local-id (albedo-texture material) pass))
  (setf (uniform program "metal_rough_occlusion_tex") (local-id (metal-rough-occlusion-texture material) pass))
  (setf (uniform program "normal_tex") (local-id (normal-texture material) pass)))

(defmethod material-block-type ((pass pbr-render-pass))
  'pbr-material-block)
