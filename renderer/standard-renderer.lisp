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
  (type :int :accessor light-type)
  (position :vec3 :accessor location)
  (direction :vec3 :accessor direction)
  (color :vec3 :accessor color)
  (attenuation-linear :float :accessor attenuation-linear)
  (attenuation-quadratic :float :accessor attenuation-quadratic)
  (outer-radius :float :accessor outer-radius)
  (cutoff-radius :float :accessor cutoff-radius))

(defmethod active-p ((light standard-light))
  (< 0 (light-type light)))

(define-gl-struct standard-light-block
  (size NIL :initarg :size :initform 128 :reader size)
  (light-count :int :accessor light-count)
  (lights (:array (:struct standard-light) size) :reader lights))

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
(defgeneric update-material (material-block material id))

(defmethod compute-shader (type (pass standard-render-pass) object)
  (if (or (typep object 'standard-renderable)
          (subtypep object 'standard-renderable))
      (let ((next (call-next-method)))
        (when next
          (list* (gl-source (material-block pass))
                 (gl-source (light-block pass))
                 next)))
      (enlist (effective-shader type object))))

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
        (let ((target (aref (slot-value struct 'lights) id)))
          (macrolet ((transfer (&rest fields)
                       `(setf ,@(loop for field in fields
                                      collect `(,field target)
                                      collect `(,field light)))))
            (transfer light-type location direction color attenuation-linear
                      attenuation-quadratic outer-radius cutoff-radius)))
        (setf (light-count struct) (max (light-count struct) (1+ id)))))))

(defmethod disable ((light standard-light) (pass standard-render-pass))
  (let ((id (lru-cache-pop light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (light-block pass))
        (setf (light-type (aref (lights struct) id)) 0)
        (loop for i downfrom (light-count struct) above 0
              do (when (active-p (aref (lights struct) i))
                   (setf (light-count struct) (1+ i))
                   (return))
              finally (setf (light-count struct) 0))))))

(defmethod local-id ((light standard-light) (pass standard-render-pass))
  (lru-cache-id light (allocated-lights pass)))

(defclass material ()
  ((textures :initform #() :accessor textures)))

(defmethod enable ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-push material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (update-material struct material id)))
    (loop for texture across (textures material)
          do (enable texture pass))))

(defmethod disable ((material material) (pass standard-render-pass))
  (lru-cache-pop material (allocated-materials pass)))

(defmethod local-id ((material material) (pass standard-render-pass))
  (lru-cache-id material (allocated-materials pass)))

(defmethod stage ((material material) (area staging-area))
  (loop for texture across (textures material)
        do (stage texture area)))

(define-shader-entity standard-renderable (renderable transformed-entity)
  ((vertex-array :initarg :vertex-array :initform NIL :accessor vertex-array))
  (:shader-file (trial "standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod stage :after ((renderable standard-renderable) (area staging-area))
  (stage (vertex-array renderable) area)
  (stage (material renderable) area))

(defmethod render ((renderable standard-renderable) (program shader-program))
  (declare (optimize speed))
  (let* ((vao (vertex-array renderable))
         (size (size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-elements (vertex-form vao) size :unsigned-int 0)))

(define-shader-entity single-material-renderable (standard-renderable)
  ((material :initarg :material :accessor material)))

(defmethod render-with :before ((pass standard-render-pass) (object single-material-renderable) program)
  (render-with pass (material object) program))

;; TODO:
;; [ ] resolve discrepancy of model import material / model "native material" <-> pass material
;;      is it reasonable to allow a model to have different materials per pass?
;; [ ] implement normal maps in phong shader
;;      dig this back out of the git history
