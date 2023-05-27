#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct pbr-material
  (textures NIL :initform (vector (// 'trial 'missing) (// 'trial 'black) (// 'trial 'black) (// 'trial 'neutral-normal)))
  (albedo-factor :vec4 :initform (vec 1 1 1 1) :initarg :albedo-factor :accessor albedo-factor)
  (emission-factor :vec4 :initform (vec 1 1 1) :initarg :emission-factor :accessor emission-factor)
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
