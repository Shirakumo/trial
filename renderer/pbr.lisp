#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct pbr-material
  (albedo-texture NIL)
  (metal-rough-occlusion-texture NIL)
  (emission-texture NIL)
  (normal-texture NIL)
  (albedo-factor :vec4)
  (emission-factor :vec4)
  (metallic-factor :float)
  (roughness-factor :float)
  (occlusion-factor :float)
  (alpha-cutoff :float))
