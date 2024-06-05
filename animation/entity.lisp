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

(define-shader-entity morphed-entity (base-animated-entity)
  ()
  (:shader-file (trial "morph.glsl")))

(defmethod render :before ((entity morphed-entity) (program shader-program))
  (declare (optimize speed))
  ;; KLUDGE: This is Bad
  (%gl:active-texture :texture6)
  (gl:bind-texture :texture-1d-array (gl-name (morph-texture entity)))
  (setf (uniform program "morph_targets") 6))

(define-shader-entity skinned-entity (base-animated-entity)
  ((mesh :initarg :mesh :initform NIL :accessor mesh))
  (:shader-file (trial "skin_matrix.glsl")))

(defmethod (setf mesh-asset) :after ((asset asset) (entity skinned-enity))
  (unless (loaded-p asset)
    (setf (palette entity) #(#.(meye 4)))))

(defmethod render :before ((entity skinned-enity) (program shader-program))
  (declare (optimize speed))
  ;; KLUDGE: This is Bad
  (bind (palette-texture entity) :texture5)
  (setf (uniform program "pose") 5))

(define-class-shader (animated-entity :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 5) in vec4 joints;
layout (location = 6) in vec4 weights;

uniform sampler1DArray pose;

out vec3 normal;
out vec4 world_pos;
out vec2 uv;

mat4 pose_matrix(in int i){
  return transpose(mat4(
    texelFetch(pose, ivec2(0, i), 0),
    texelFetch(pose, ivec2(1, i), 0),
    texelFetch(pose, ivec2(2, i), 0),
    vec4(0,0,0,1)));
}

void main(){
  ivec4 j = ivec4(joints);
  mat4 skin_matrix = (pose_matrix(j.x) * weights.x)
                   + (pose_matrix(j.y) * weights.y)
                   + (pose_matrix(j.z) * weights.z)
                   + (pose_matrix(j.w) * weights.w);
  world_pos = model_matrix * skin_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * skin_matrix * vec4(in_normal, 0.0f));
  uv = in_uv;
  gl_Position = projection_matrix * view_matrix * world_pos;
}")

(defclass quat2-animation-controller (animation-controller)
  ())

(defmethod update-palette ((entity quat2-animation-controller))
  ;; FIXME: Update for texture data
  (let ((palette (quat2-palette (pose entity) (palette entity)))
        (inv (quat-inv-bind-pose (skeleton (mesh-asset entity)))))
    (dotimes (i (length palette) (setf (palette entity) palette))
      (nq* (svref palette i) (svref inv i)))))

(define-shader-entity quat2-skinned-enity (base-animated-enity)
  ()
  (:shader-file (trial "skin_dquat.glsl")))

(defmethod render :before ((entity quat2-skinned-enity) (program shader-program))
  (declare (optimize speed))
  ;; KLUDGE: This is Bad
  (bind (palette-texture entity) :texture5)
  (setf (uniform program "pose") 5))

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
