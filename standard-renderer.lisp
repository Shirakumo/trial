#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: would be nice to have this dynamically configurable....
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
   (frame-start :initform 0d0 :accessor frame-start)
   (allocated-textures :initform (make-lru-cache 16 'eq) :accessor allocated-textures)
   (allocated-materials :initform (make-lru-cache MAX-MATERIALS 'eq) :accessor allocated-materials)
   (allocated-lights :initform (make-lru-cache MAX-LIGHTS 'eq) :accessor allocated-lights))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "standard-render-pass.glsl")))

(defmethod shared-initialize :after ((pass standard-render-pass) slots &key)
  (let ((max-textures (gl:get-integer :max-texture-image-units)))
    (dolist (port (flow:ports pass))
      (typecase port
        (texture-port
         (setf max-textures (max max-textures (unit-id port))))))
    (lru-cache-resize (allocated-textures pass) max-textures)))

(define-handler (standard-render-pass tick) (tt dt)
  (with-buffer-tx (buffer (// 'trial 'standard-environment-information) :update NIL)
    (setf (slot-value buffer 'tt) (float tt 0f0))
    (setf (slot-value buffer 'dt) (float dt 0f0))))

(defmethod render :before ((pass standard-render-pass) target)
  (let* ((frame-time (current-time))
         (old-time (shiftf (frame-start pass) frame-time))
         (fdt (- old-time frame-time)))
    (with-buffer-tx (buffer (// 'trial 'standard-environment-information))
      (setf (slot-value buffer 'view-matrix) (view-matrix))
      (setf (slot-value buffer 'projection-matrix) (projection-matrix))
      (setf (slot-value buffer 'view-size) (vec2 (width (framebuffer pass)) (height (framebuffer pass))))
      (setf (slot-value buffer 'camera-position) (location (camera pass)))
      (setf (slot-value buffer 'fdt) (float fdt 0f0)))))

(defmethod bind-textures ((pass standard-render-pass))
  (call-next-method)
  (do-lru-cache (texture id (allocated-textures pass))
    (gl:active-texture id)
    (gl:bind-texture :texture-2d (gl-name texture))))

(defmethod enable ((texture texture) (pass standard-render-pass))
  (let ((id (lru-cache-push texture (allocated-textures pass))))
    (when id
      (gl:active-texture id)
      (gl:bind-texture :texture-2d (gl-name texture)))))

(defmethod disable ((texture texture) (pass standard-render-pass))
  (lru-cache-pop texture (allocated-textures pass)))

(defmethod local-id ((texture texture) (pass standard-render-pass))
  (lru-cache-id texture (allocated-textures pass)))

(defmethod enable ((light standard-light) (pass standard-render-pass))
  (let ((id (lru-cache-push light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (// 'trial 'standard-light-block))
        (setf (aref (slot-value struct 'lights) id) light)))))

(defmethod disable ((light standard-light) (pass standard-render-pass))
  (lru-cache-pop light (allocated-lights pass)))

(defmethod local-id ((light standard-light) (pass standard-render-pass))
  (lru-cache-id light (allocated-lights pass)))

(defclass material ()
  ())

(defmethod enable ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-push material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (setf (elt (slot-value struct 'materials) id) material)))))

(defmethod disable ((material material) (pass standard-render-pass))
  (lru-cache-pop material (allocated-materials pass)))

(defmethod local-id ((material material) (pass standard-render-pass))
  (lru-cache-id material (allocated-materials pass)))

(define-shader-entity standard-renderable (renderable transformed-entity)
  ((material :initarg :material :accessor material))
  (:shader-file (trial "standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod render-with :before ((pass standard-render-pass) (object standard-renderable) program)
  (enable (material object) pass)
  (prepare-pass-program material program))

(define-gl-struct phong-material
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
  ((material-block :initform (// 'trial 'phong-material-block) :accessor material-block))
  (:shader-file (trial "standard-render-phong.glsl"))
  (:buffers (trial phong-material-block)))

(defmethod render-with :before ((pass phong-render-pass) (object standard-renderable) program)
  (setf (uniform program "material_id") (local-id (material object) pass)))

(defmethod prepare-pass-program ((material phong-material) program)
  (setf (uniform program "diffuse_tex") (diffuse-texture material))
  (setf (uniform program "specular_tex") (specular-texture material)))
