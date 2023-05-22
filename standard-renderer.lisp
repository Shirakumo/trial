#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defconstant MAX-LIGHTS 128)
(defconstant MAX-MATERIALS 64)

(define-gl-struct standard-light
  (type :int)
  (position :vec3)
  (direction :vec3)
  (color :vec3)
  (attenuation-linear :float)
  (attenuation-quadratic :float)
  (outer-radius :float)
  (cutoff-radius :float))

(define-gl-struct standard-light-block
  (lights (:array (:struct standard-light) #.MAX-LIGHTS))
  (light-count :int))

(define-asset (trial standard-light-block) uniform-block
    'standard-light-block
  :binding NIL)

(define-gl-struct standard-textures
  (albedo :int)
  (normal :int)
  (emission :int)
  (metal-rough-occlusion :int))

(define-gl-struct standard-material
  (textures (:array :int 8))
  (albedo-factor :vec4)
  (emission-factor :vec4)
  (metallic-factor :float)
  (roughness-factor :float)
  (occlusion-factor :float)
  (alpha-cutoff :float))

(define-gl-struct standard-material-block
  (materials (:array (:struct standard-material) #.MAX-MATERIALS))
  (material-count :int))

(define-asset (trial standard-material-block) uniform-block
    'standard-material-block
  :binding NIL)

(define-gl-struct standard-environment-information
  (view-matrix :mat4)
  (projection-matrix :mat4)
  (view-size :vec2)
  (camera-position :vec3)
  (tt :float)
  (dt :float)
  (fdt :float))

(define-asset (trial standard-environment-information) uniform-block
    'standard-environment-information
  :binding NIL)

(define-shader-pass standard-render-pass (per-object-pass)
  ((color :port-type output :texspec (:internal-format :rgba32f) :attachment :color-attachment0 :reader color)
   (normal :port-type output :texspec (:internal-format :rgb16f) :attachment :color-attachment1 :reader normal)
   (depth :port-type output :attachment :depth-stencil-attachment :reader depth)
   (tt :initform 0.0 :accessor tt)
   (dt :initform 0.0 :accessor dt)
   (frame-start :initform 0d0 :accessor frame-start))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "standard-render-pass.glsl")))

(define-handler (standard-render-pass tick) (dt)
  (let ((dt (float dt 0f0)))
    (setf (dt standard-render-pass) dt)
    (setf (tt standard-render-pass) (+ (tt standard-render-pass) dt))))

(defmethod render :before ((pass standard-render-pass) target)
  (let* ((frame-time (current-time))
         (old-time (shiftf (frame-start pass) frame-time))
         (fdt (- old-time frame-time)))
    (with-buffer-tx (buffer (// 'trial 'standard-environment-information))
      (setf (slot-value buffer 'view-matrix) (view-matrix))
      (setf (slot-value buffer 'projection-matrix) (projection-matrix))
      (setf (slot-value buffer 'view-size) (vec2 (width (framebuffer pass)) (height (framebuffer pass))))
      (setf (slot-value buffer 'camera-position) (location (camera pass)))
      (setf (slot-value buffer 'tt) (tt pass))
      (setf (slot-value buffer 'dt) (dt pass))
      (setf (slot-value buffer 'fdt) (float fdt 0f0)))))

(define-shader-entity standard-renderable (renderable transformed-entity)
  ((material :initarg :material :accessor material))
  (:shader-file (trial "standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))
