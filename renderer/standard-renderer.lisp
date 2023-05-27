#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial missing) image
    #p "missing.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial black) image
    #p "black.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-asset (trial neutral-normal) image
    #p "neutral-normal.png"
  :min-filter :nearest
  :mag-filter :nearest)

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
  (size NIL :initarg :size :initform 128 :reader size)
  (light-count :int)
  (lights (:array (:struct standard-light) size)))

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
   (material-block :reader material-block)
   (light-block :reader light-block)
   (allocated-textures :initform (make-lru-cache 16 'eq) :accessor allocated-textures)
   (allocated-materials :accessor allocated-materials)
   (allocated-lights :accessor allocated-lights))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "standard-render-pass.glsl")))

(defmethod initialize-instance :after ((pass standard-render-pass) &key (max-lights 128) (max-materials 64))
  (setf (allocated-materials pass) (make-lru-cache max-materials))
  (setf (allocated-lights pass) (make-lru-cache max-lights))
  (setf (slot-value pass 'material-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance (material-block-type pass) :size max-materials)))
  (setf (slot-value pass 'light-block) (make-instance 'uniform-buffer :binding NIL :struct (make-instance 'standard-light-block :size max-lights))))

(defmethod shared-initialize :after ((pass standard-render-pass) slots &key)
  (let ((max-textures (max 16 (gl:get-integer :max-texture-image-units))))
    (dolist (port (flow:ports pass))
      (typecase port
        (texture-port
         (setf max-textures (max max-textures (unit-id port))))))
    (lru-cache-resize (allocated-textures pass) max-textures)))

(defgeneric material-block-type (standard-render-pass))

(defmethod compute-shader append (type (pass standard-render-pass) object)
  (list (gl-source (material-block pass))
        (gl-source (light-block pass))))

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

(defmethod buffers ((pass standard-render-pass))
  (list* (material-block pass) (light-block pass) (call-next-method)))

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
      (with-buffer-tx (struct (light-block pass))
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

;; TODO:
;; [ ] better mechanism for define-gl-struct, something with referential transparency so it can be "deallocated" out of a uniform block
;;      just create an opaque static-vector-backed UBO and add a conversion to a specified index? maybe via (setf elt) on a gl-struct?
;; [ ] resolve discrepancy of model import material / model "native material" <-> pass material
;;      is it reasonable to allow a model to have different materials per pass?
;; [ ] implement normal maps in phong shader
;;      dig this back out of the git history
