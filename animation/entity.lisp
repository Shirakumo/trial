(in-package #:org.shirakumo.fraf.trial)

(defclass base-animated-entity (mesh-entity)
  ((animation-controller :initform (make-instance 'animation-controller) :accessor animation-controller)))

(define-transfer base-animated-entity animation-controller)

(define-accessor-wrapper-methods clip (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods skeleton (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods pose (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette-texture (base-animated-entity (animation-controller base-animated-entity)))

(defmethod (setf mesh-asset) :after ((asset asset) (entity base-animated-entity))
  (setf (model (animation-controller entity)) asset))

(defmethod stage :after ((entity base-animated-entity) (area staging-area))
  (register-load-observer area (animation-controller entity) (mesh-asset entity))
  (stage (animation-controller entity) area))

(defmethod play (thing (entity base-animated-entity))
  (play thing (animation-controller entity)))

(defmethod fade-to (thing (entity base-animated-entity) &rest args &key &allow-other-keys)
  (apply #'fade-to thing (animation-controller entity) args))

(defmethod add-layer (thing (entity base-animated-entity) &rest args)
  (apply #'add-layer thing (animation-controller entity) args))

(defmethod find-clip (thing (entity base-animated-entity) &optional (errorp T))
  (find-clip thing (animation-controller entity) errorp))

(defmethod list-clips ((entity base-animated-entity))
  (list-clips entity))

(define-shader-entity armature (base-animated-entity lines)
  ((color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(define-handler ((entity armature) (ev tick) :after) ()
  (replace-vertex-data entity (pose entity) :default-color (color entity)))

(define-gl-struct morph-data
  (size NIL :initarg :size :initform 8 :reader size)
  (morph-count :int :initform 0 :accessor morph-count :reader sequence:length)
  (weights (:array :float size))
  (indices (:array :int size)))

(define-shader-entity morphed-entity (base-animated-entity)
  ((morph-texture :initform NIL :accessor morph-texture)
   (morph-data :buffer T :initform (make-instance 'uniform-buffer :data-usage :dynamic-draw :binding NIL :struct (make-instance 'morph-data)) :accessor morph-data))
  (:shader-file (trial "morph.glsl")))

(defmethod render :before ((entity morphed-entity) (program shader-program))
  (declare (optimize speed))
  ;; KLUDGE: This is Bad
  (when (morph-texture entity)
    (bind (morph-texture entity) :texture6)
    (setf (uniform program "morph_targets") 6)))

(defmethod stage :after ((entity morphed-entity) (area staging-area))
  (stage (morph-texture entity) area)
  (stage (morph-data entity) area))

(define-shader-entity skinned-entity (base-animated-entity)
  ((mesh :initarg :mesh :initform NIL :accessor mesh))
  (:shader-file (trial "skin-matrix.glsl")))

(defmethod (setf mesh-asset) :after ((asset asset) (entity skinned-enity))
  (unless (loaded-p asset)
    (setf (palette entity) #(#.(meye 4)))))

(defmethod render :before ((entity skinned-entity) (program shader-program))
  (declare (optimize speed))
  (when (palette-texture entity)
    (bind (palette-texture entity) :texture5)
    (setf (uniform program "pose") 5)))

(define-shader-entity quat2-skinned-entity (base-animated-entity)
  ()
  (:shader-file (trial "skin-dquat.glsl")))

(defmethod render :before ((entity quat2-skinned-entity) (program shader-program))
  (declare (optimize speed))
  (when (palette-texture entity)
    (bind (palette-texture entity) :texture5)
    (setf (uniform program "pose") 5)))

(define-shader-entity animated-entity (skinned-entity morphed-entity)
  ())

(define-class-shader (animated-entity :vertex-shader)
  "layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 5) in vec4 in_joints;
layout (location = 6) in vec4 in_weights;

uniform mat2x4 pose[120];

out vec3 normal;
out vec4 world_pos;
out vec2 uv;

void main(){
  vec3 position = in_position;
  normal = in_normal;
  uv = in_uv;

  morph_vertex(position, normal, uv);
  skin_vertex(position, normal, in_joints, in_weights);

  world_pos = model_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * vec4(normal, 0.0f));
  gl_Position = projection_matrix * view_matrix * world_pos;
}")
