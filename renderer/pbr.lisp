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

(defmethod shared-initialize :after ((material pbr-material) slots &key metalness-texture roughness-texture occlusion-texture metal-rough-texture)
  (cond ((and metalness-texture roughness-texture occlusion-texture)
         (loop for source in (sources metalness-texture)
               do (setf (target source) :r))
         (loop for source in (sources roughness-texture)
               do (setf (target source) :g))
         (loop for source in (sources occlusion-texture)
               do (setf (target source) :b))
         (setf (aref (textures material) 1) (merge-textures (list metalness-texture roughness-texture occlusion-texture))))
        ((and metalness-texture roughness-texture)
         (setf (occlusion-factor material) 0.0)
         (loop for source in (sources metalness-texture)
               do (setf (target source) :r))
         (loop for source in (sources roughness-texture)
               do (setf (target source) :g))
         (setf (aref (textures material) 1) (merge-textures (list metalness-texture roughness-texture))))
        ((and metal-rough-texture occlusion-texture)
         (loop for source in (sources metal-rough-texture)
               do (setf (target source) :rg))
         (loop for source in (sources occlusion-texture)
               do (setf (target source) :b))
         (setf (aref (textures material) 1) (merge-textures (list metal-rough-texture occlusion-texture))))
        ((and metal-rough-texture)
         (setf (occlusion-factor material) 0.0)
         (setf (aref (textures material) 1) metal-rough-texture))
        ((and metalness-texture)
         (setf (occlusion-factor material) 0.0)
         (setf (roughness-factor material) 0.0)
         (setf (aref (textures material) 1) metalness-texture))))

(defmethod texture-names ((material pbr-material))
  #(:albedo-texture :metal-rough-occlusion-texture :emission-texture :normal-texture))

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
  (let ((textures (textures material)))
    (setf (uniform program "material_id") (local-id material pass))
    (setf (uniform program "albedo_tex") (local-id (aref textures 0) pass))
    (setf (uniform program "metal_rough_occlusion_tex") (local-id (aref textures 1) pass))
    (setf (uniform program "emission_tex") (local-id (aref textures 2) pass))
    (setf (uniform program "normal_tex") (local-id (aref textures 3) pass))))

(defmethod material-block-type ((pass pbr-render-pass))
  'pbr-material-block)
