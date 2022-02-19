#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(trial:define-shader-entity entity (trial:transformed-entity trial:renderable trial:listener)
  ((vertex-array :initarg :vertex-array :accessor trial:vertex-array)
   (palette :initform #() :accessor palette)
   (clock :initform 0.0 :accessor clock)
   (mesh :initarg :mesh :accessor mesh)
   (asset :initarg :asset :accessor asset)
   (clip :initarg :clip :initform NIL :accessor clip)
   (pose :accessor pose)))

(defmethod initialize-instance :after ((entity entity) &key (mesh 0))
  (trial:register-generation-observer entity (asset entity))
  (setf (trial:vertex-array entity) (trial:resource (asset entity) mesh)))

(defmethod trial:stage :after ((entity entity) (area trial:staging-area))
  (trial:stage (asset entity) area)
  (trial:stage (trial:vertex-array entity) area))

(defmethod trial:observe-generation ((entity entity) (asset gltf-asset) res)
  (setf (asset entity) asset))

(defmethod (setf asset) :after ((asset gltf-asset) (entity entity))
  (setf (mesh entity) (gethash 0 (meshes asset)))
  (setf (clip entity) (svref (clips asset) 0))
  (setf (pose entity) (make-instance 'pose :source (rest-pose (skeleton asset)))))

(defmethod (setf mesh) :after ((mesh mesh) (entity entity))
  (setf (trial:vertex-array entity) (trial:resource (asset entity) (trial:name mesh))))

(defmethod trial:handle ((ev trial:tick) (entity entity))
  (let ((dt (trial:dt ev)))
    (setf (clock entity) (sample-pose (clip entity) (pose entity) (+ (clock entity) dt)))
    (let ((palette (matrix-palette (pose entity)))
          (inv (inv-bind-pose (skeleton (asset entity)))))
      (dotimes (i (length palette))
        (nm* (svref palette i) (svref inv i)))
      (setf (palette entity) palette))))

(defmethod trial:render ((entity entity) (program trial:shader-program))
  (declare (optimize speed))
  (when (< 0 (length (palette entity)))
    (setf (trial:uniform program "pose[0]") (svref (palette entity) 0)))
  (when (< 1 (length (palette entity)))
    (setf (trial:uniform program "pose[1]") (svref (palette entity) 1)))
  (when (< 2 (length (palette entity)))
    (setf (trial:uniform program "pose[2]") (svref (palette entity) 2)))
  (when (< 3 (length (palette entity)))
    (setf (trial:uniform program "pose[3]") (svref (palette entity) 3)))
  (setf (trial:uniform program "model_matrix") (trial:model-matrix))
  (setf (trial:uniform program "view_matrix") (trial:view-matrix))
  (setf (trial:uniform program "projection_matrix") (trial:projection-matrix))
  (let* ((vao (trial:vertex-array entity))
         (size (trial:size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (trial:gl-name vao))
    ;; KLUDGE: Bad for performance!
    (if (loop for binding in (trial:bindings vao)
              thereis (typep binding 'trial:vertex-buffer))
        (%gl:draw-elements :triangles size :unsigned-int 0)
        (%gl:draw-arrays :triangles 0 size))
    (gl:bind-vertex-array 0)))

(trial:define-class-shader (entity :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_texcoord;
layout (location = 3) in vec4 joints;
layout (location = 4) in vec4 weights;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

uniform mat4 pose[4];

out vec3 normal;
out vec4 world_pos;
out vec2 texcoord;

void main(){
  ivec4 j = ivec4(joints);
  mat4 skin_matrix = (pose[j.x] * weights.x)
                   + (pose[j.y] * weights.y)
                   + (pose[j.z] * weights.z)
                   + (pose[j.w] * weights.w);
  skin_matrix = mat4(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1);
  world_pos = model_matrix * skin_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * skin_matrix * vec4(in_normal, 0.0f));
  texcoord = in_texcoord;
  gl_Position = projection_matrix * view_matrix * world_pos;
}")
