#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: would be nice to have this dynamically configurable....
(defconstant MAX-LIGHTS 128)
(defconstant MAX-MATERIALS 64)
(defconstant MAX-TEXTURES 32)

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
   (frame-start :initform 0d0 :accessor frame-start)
   (allocated-materials :initform (make-lru-cache MAX-MATERIALS) :accessor allocated-materials)
   (allocated-textures :initform (make-lru-cache MAX-TEXTURES) :accessor allocated-textures)
   (allocated-lights :initform (make-lru-cache MAX-LIGHTS) :accessor allocated-lights))
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

(define-gl-struct phong-material
  (textures (:array :int 3))
  (diffuse-factor :vec4)
  (specular-factor :vec3)
  (alpha-cutoff :float))

(define-gl-struct phong-material-block
  (materials (:array (:struct phong-material) #.MAX-MATERIALS))
  (material-count :int))

(define-asset (trial phong-material-block) uniform-block
    'standard-material-block
  :binding NIL)

(define-shader-pass phong-render-pass (standard-render-pass)
  ()
  (:shader-file (trial "standard-render-phong.glsl"))
  (:buffers (trial phong-material-block)))

(define-shader-entity standard-renderable (renderable transformed-entity)
  ((material :initarg :material :accessor material))
  (:shader-file (trial "standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod enable ((material material) (pass standard-render-pass))
  (multiple-value-bind (id pushed) (lru-cache-push material (allocated-materials pass))
    (when pushed
      (dolist (texture (textures material))
        (multiple-value-bind (id pushed) (lru-cache-push texture (allocated-textures pass))
          (when pushed
            ;; FIXME: implement this
            ;; We have to manually evict all materials that use the texture ID that was evicted.
            (dolist (material (texture-materials id pass))
              (lru-cache-pop material (allocated-materials pass)))
            (setf (texture-materials id pass) (list material))
            (gl:active-texture id)
            (gl:bind-texture :texture-2d (gl-name texture))
            (setf (unit-id texture) id))))
      (with-buffer-tx (struct (material-block pass))
        (setf (aref (slot-value struct 'materials) id) material)))
    id))

(defmethod enable ((light standard-light) (pass standard-render-pass))
  (multiple-value-bind (id pushed) (lru-cache-push light (allocated-lights pass))
    (when pushed
      (with-buffer-tx (struct (// 'trial 'standard-light-block))
        (setf (aref (slot-value struct 'lights) id) light)))))

(defmethod render-with :before ((pass standard-render-pass) (object standard-renderable) program)
  (setf (uniform program "object_material") (enable (material object) pass)))
