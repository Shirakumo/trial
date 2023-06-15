#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct standard-environment-information
  (view-matrix :mat4)
  (projection-matrix :mat4)
  (view-size :vec2 :accessor view-size)
  (camera-position :vec3 :accessor location)
  (tt :float :accessor tt)
  (dt :float :accessor dt)
  (fdt :float :accessor fdt))

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
         (setf max-textures (min max-textures (unit-id port))))))
    (lru-cache-resize (allocated-textures pass) max-textures)))

(defmethod clear :after ((pass standard-render-pass))
  (lru-cache-clear (allocated-textures pass))
  (lru-cache-clear (allocated-materials pass))
  (lru-cache-clear (allocated-lights pass)))

(defgeneric material-block-type (standard-render-pass))

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
         (fdt (- frame-time old-time)))
    (with-buffer-tx (buffer (// 'trial 'standard-environment-information))
      (setf (slot-value buffer 'view-matrix) (view-matrix))
      (setf (slot-value buffer 'projection-matrix) (projection-matrix))
      (setf (slot-value buffer 'view-size) (vec2 (width (framebuffer pass)) (height (framebuffer pass))))
      (setf (slot-value buffer 'camera-position) (global-location (camera pass)))
      (setf (slot-value buffer 'fdt) (float fdt 0f0)))))

(defmethod buffers ((pass standard-render-pass))
  (list* (material-block pass) (light-block pass) (call-next-method)))

(defmethod bind-textures ((pass standard-render-pass))
  (call-next-method)
  (do-lru-cache (texture id (allocated-textures pass))
    (gl:active-texture id)
    (gl:bind-texture (target texture) (gl-name texture))))

(defmethod enable ((texture texture) (pass standard-render-pass))
  (let ((id (lru-cache-push texture (allocated-textures pass))))
    (when id
      (gl:active-texture id)
      (gl:bind-texture (target texture) (gl-name texture)))))

(defmethod disable ((texture texture) (pass standard-render-pass))
  (lru-cache-pop texture (allocated-textures pass)))

(defmethod local-id ((texture texture) (pass standard-render-pass))
  (lru-cache-id texture (allocated-textures pass)))

(defmethod enable ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-push light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (light-block pass))
        (transfer-to (aref (slot-value struct 'lights) id) light)
        (setf (light-count struct) (max (light-count struct) (1+ id)))))))

(defmethod disable ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-pop light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (light-block pass))
        (setf (light-type (aref (lights struct) id)) 0)
        (loop for i downfrom (light-count struct) above 0
              do (when (active-p (aref (lights struct) i))
                   (setf (light-count struct) (1+ i))
                   (return))
              finally (setf (light-count struct) 0))))))

(defmethod local-id ((light light) (pass standard-render-pass))
  (lru-cache-id light (allocated-lights pass)))

(defmethod notice-update ((light light) (pass standard-render-pass))
  (let ((id (lru-cache-id light (allocated-lights pass))))
    (when id
      (with-buffer-tx (struct (light-block pass))
        (transfer-to (aref (slot-value struct 'lights) id) light)))))

(defmethod enable ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-push material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (transfer-to (aref (slot-value struct 'materials) id) material)))
    (loop for texture across (textures material)
          do (enable texture pass))))

(defmethod disable ((material material) (pass standard-render-pass))
  (lru-cache-pop material (allocated-materials pass)))

(defmethod local-id ((material material) (pass standard-render-pass))
  (lru-cache-id material (allocated-materials pass)))

(defmethod notice-update ((material material) (pass standard-render-pass))
  (let ((id (lru-cache-id material (allocated-materials pass))))
    (when id
      (with-buffer-tx (struct (material-block pass))
        (transfer-to (aref (slot-value struct 'materials) id) material)))))

(defmethod render-with ((pass standard-render-pass) (material material) program)
  (error "Unsupported material~%  ~s~%for pass~%  ~s"
         material pass))

(define-shader-entity standard-renderable (renderable)
  ((vertex-array :initarg :vertex-array :initform NIL :accessor vertex-array))
  (:shader-file (trial "standard-renderable.glsl"))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod stage :after ((renderable standard-renderable) (area staging-area))
  (stage (vertex-array renderable) area))

(defmethod render ((renderable standard-renderable) (program shader-program))
  (declare (optimize speed))
  (let* ((vao (vertex-array renderable))
         (size (size vao)))
    (declare (type (unsigned-byte 32) size))
    (setf (uniform program "model_matrix") (model-matrix))
    (setf (uniform program "inv_model_matrix") (minv (model-matrix)))
    (gl:bind-vertex-array (gl-name vao))
    (if (indexed-p vao)
        (%gl:draw-elements (vertex-form vao) size :unsigned-int 0)
        (%gl:draw-arrays (vertex-form vao) 0 size))))

(define-shader-entity single-material-renderable (standard-renderable)
  ((material :initarg :material :accessor material)))

(defmethod stage :after ((renderable single-material-renderable) (area staging-area))
  (when (material renderable)
    (stage (material renderable) area)))

(defmethod render-with :before ((pass standard-render-pass) (object single-material-renderable) program)
  (prepare-pass-program pass program)
  (render-with pass (material object) program))

(define-shader-pass light-cache-render-pass (standard-render-pass)
  ((light-cache :initform (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree) :reader light-cache)
   (light-cache-dirty-p :initform T :accessor light-cache-dirty-p)
   (light-cache-location :initform (vec 0 0 0) :reader light-cache-location)
   (light-cache-distance-threshold :initform 10.0 :accessor light-cache-distance-threshold)
   (ambient-light :initform NIL :accessor ambient-light)))

(defmethod object-renderable-p ((light light) (pass light-cache-render-pass)) T)

(defmethod clear :after ((pass light-cache-render-pass))
  (3ds:clear (light-cache pass)))

(defmethod enter ((light light) (pass light-cache-render-pass))
  (3ds:enter light (light-cache pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod leave ((light light) (pass light-cache-render-pass))
  (3ds:leave light (light-cache pass))
  (setf (light-cache-dirty-p pass) T))

(defmethod enter ((light ambient-light) (pass light-cache-render-pass))
  (setf (ambient-light pass) light)
  (setf (light-cache-dirty-p pass) T))

(defmethod leave ((light ambient-light) (pass light-cache-render-pass))
  (when (eq light (ambient-light pass))
    (setf (ambient-light pass) NIL)
    (disable light pass)))

(define-handler ((pass light-cache-render-pass) tick :before) ()
  (when (<= (light-cache-distance-threshold pass)
            (vsqrdistance (focal-point (camera pass)) (light-cache-location pass)))
    (setf (light-cache-dirty-p pass) T)))

(defmethod render :before ((pass light-cache-render-pass) target)
  (when (light-cache-dirty-p pass)
    (let ((location (v<- (light-cache-location pass) (focal-point (camera pass))))
          (size (1- (lru-cache-size (allocated-lights pass)))))
      (multiple-value-bind (nearest count) (org.shirakumo.fraf.trial.space.kd-tree:kd-tree-k-nearest
                                            size location (light-cache pass) :test #'active-p)
        (dotimes (i count)
          (enable (aref nearest i) pass)))
      (when (ambient-light pass)
        (enable (ambient-light pass) pass)))
    (setf (light-cache-dirty-p pass) NIL)))

;; FIXME: how do we know when lights moved or de/activated so we can update?
