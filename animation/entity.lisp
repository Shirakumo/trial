#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(defstruct (animation-layer
            (:constructor %make-animation-layer (clip pose base)))
  (clip NIL :type clip)
  (pose NIL :type pose)
  (base NIL :type pose)
  (strength 0.0 :type single-float))

(defun make-animation-layer (clip skeleton &key (strength 0.0))
  (let ((layer (%make-animation-layer
                clip
                (rest-pose* skeleton)
                (instantiate-clip skeleton clip))))
    (setf (strength layer) strength)
    layer))

(defmethod strength ((layer animation-layer))
  (animation-layer-strength layer))

(defmethod (setf strength) (strength (layer animation-layer))
  (let ((clip (animation-layer-clip layer))
        (strength (trial:clamp 0.0 (float strength 0f0) 1.0)))
    (sample-pose clip (animation-layer-pose layer) (+ (start-time clip) (* strength (duration clip))))
    (setf (animation-layer-strength layer) strength)))

(defclass layer-controller ()
  ((layers :initform (make-hash-table :test 'equalp) :accessor layers)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod update ((controller layer-controller) dt)
  (when (next-method-p) (call-next-method))
  (loop for layer being the hash-values of (layers controller)
        do (layer-onto (pose controller) (pose controller) (animation-layer-pose layer) (animation-layer-base layer))))

(defmethod add-layer ((layer animation-layer) (controller layer-controller) &key name)
  (setf (gethash name (layers controller)) layer))

(defmethod add-layer ((clip clip) (controller layer-controller) &key (strength 0.0) (name (trial:name clip)))
  (setf (gethash name (layers controller)) (make-animation-layer clip (skeleton controller) :strength strength)))

(defmethod remove-layer (name (controller layer-controller))
  (remhash name (layers controller)))

(defmethod layer (name (controller layer-controller))
  (gethash name (layers controller)))

(defstruct (fade-target
            (:constructor make-fade-target (clip pose duration)))
  (pose NIL :type pose)
  (clip NIL :type clip)
  (clock 0.0 :type single-float)
  (duration 0.0 :type single-float)
  (elapsed 0.0 :type single-float))

(defclass fade-controller ()
  ((targets :initform (make-array 0 :adjustable T :fill-pointer T) :accessor targets)
   (clip :initarg :clip :initform NIL :accessor clip)
   (clock :initform 0.0 :accessor clock)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod shared-initialize :after ((controller fade-controller) slots &key skeleton)
  (when skeleton
    (setf (skeleton controller) skeleton)))

(defmethod (setf skeleton) :after ((skeleton skeleton) (controller fade-controller))
  (setf (pose controller) (rest-pose* skeleton)))

(defmethod play ((target clip) (controller fade-controller))
  (setf (fill-pointer (targets controller)) 0)
  (setf (clip controller) target)
  (pose<- (pose controller) (rest-pose (skeleton controller)))
  (setf (clock controller) (start-time target))
  (sample-pose (clip controller) (pose controller) (clock controller)))

(defmethod fade-to ((target clip) (controller fade-controller) &key (duration 0.2))
  (let ((targets (targets controller)))
    (cond ((null (clip controller))
           (play target controller))
          ((and (or (= 0 (length targets))
                    (not (eq target (fade-target-clip (aref targets (1- (length targets)))))))
                (not (eq target (clip controller))))
           (vector-push-extend (make-fade-target target (rest-pose (skeleton controller)) duration) targets)))))

(defmethod update ((controller fade-controller) dt)
  (when (next-method-p) (call-next-method))
  (when (and (clip controller) (skeleton controller))
    (let ((targets (targets controller)))
      (loop for target across targets
            for i from 0
            do (when (<= (fade-target-duration target) (fade-target-elapsed target))
                 (setf (clip controller) (fade-target-clip target))
                 (setf (clock controller) (fade-target-clock target))
                 (pose<- (pose controller) (fade-target-pose target))
                 (array-utils:vector-pop-position targets i)
                 (return)))
      (let ((time (sample-pose (clip controller) (pose controller) (+ (clock controller) dt))))
        (setf (clock controller) time)
        (loop for target across targets
              do (setf (fade-target-clock target) (sample-pose (fade-target-clip target) (fade-target-pose target) (+ (fade-target-clock target) dt)))
                 (incf (fade-target-elapsed target) dt)
                 (let ((time (min 1.0 (/ (fade-target-elapsed target) (fade-target-duration target)))))
                   (blend-into (pose controller) (pose controller) (fade-target-pose target) time)))))))

(trial:define-shader-entity armature (fade-controller trial:lines trial:listener)
  ((asset :initarg :asset :accessor asset)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(defmethod initialize-instance :after ((entity armature) &key asset)
  (trial:register-generation-observer entity asset))

(defmethod trial:stage :after ((entity armature) (area trial:staging-area))
  (trial:stage (asset entity) area))

(defmethod trial:observe-generation ((entity armature) (asset gltf-asset) res)
  (setf (skeleton entity) (skeleton asset))
  (typecase (clip entity)
    (string (play (gethash (clip entity) (clips (asset entity))) entity))
    ((eql T) (play (loop for v being the hash-values of (clips (asset entity)) return v) entity))
    (null (setf (pose entity) (rest-pose* (skeleton asset))))))

(defmethod trial:handle ((ev trial:tick) (entity armature))
  (when (pose entity)
    (when (trial:retained :space)
      (update entity (trial:dt ev)))
    (when (trial:retained :backspace)
      (when (clip entity)
        (play (clip entity) entity)))
    (trial:replace-vertex-data entity (pose entity) :default-color (color entity))))

(trial:define-shader-entity entity (fade-controller layer-controller trial:transformed-entity trial:renderable trial:listener)
  ((vertex-array :initarg :vertex-array :accessor trial:vertex-array)
   (texture :initarg :texture :accessor trial:texture)
   (palette :initform #() :accessor palette)
   (mesh :initarg :mesh :initform NIL :accessor mesh)
   (asset :initarg :asset :accessor asset)))

(defmethod initialize-instance :after ((entity entity) &key)
  (trial:register-generation-observer entity (asset entity)))

(defmethod trial:stage :after ((entity entity) (area trial:staging-area))
  ;; FIXME: fuck. don't know how to load the resources?
  (trial:stage (asset entity) area))

(defmethod trial:observe-generation ((entity entity) (asset gltf-asset) res)
  (setf (asset entity) asset))

(defmethod (setf asset) :after ((asset gltf-asset) (entity entity))
  (setf (mesh entity) (or (mesh entity) T))
  (if (skeleton asset)
      (setf (skeleton entity) (skeleton asset))
      (setf (palette entity) #(#.(meye 4))))
  (play (or (clip entity) T) entity))

(defmethod fade-to ((name string) (entity entity) &rest args)
  (let ((clip (gethash name (clips (asset entity)))))
    (if clip
        (apply #'fade-to clip entity args)
        #-trial-release
        (error "No animation clip named ~s found." name))))

(defmethod play ((name string) (entity entity))
  (let ((clip (gethash name (clips (asset entity)))))
    (if clip
        (play clip entity)
        #-trial-release
        (error "No animation clip named ~s found." name))))

(defmethod play ((anything (eql T)) (entity entity))
  (loop for clip being the hash-values of (clips (asset entity))
        do (return (play clip entity))))

(defmethod (setf pose) :after ((pose pose) (entity entity))
  (update-palette entity))

(defmethod (setf mesh) :after ((mesh mesh) (entity entity))
  (setf (trial:vertex-array entity) (trial:resource (asset entity) (trial:name mesh)))
  (setf (trial:texture entity) (trial:texture mesh)))

(defmethod (setf mesh) ((name string) (entity entity))
  (let ((mesh (gethash name (meshes (asset entity)))))
    (if mesh
        (setf (mesh entity) mesh)
        #-trial-release
        (error "No mesh named ~s found." name))))

(defmethod (setf mesh) ((anything (eql T)) (entity entity))
  (loop for mesh being the hash-values of (meshes (asset entity))
        do (return (setf (mesh entity) mesh))))

(defun update-palette (entity)
  (let ((palette (matrix-palette (pose entity) (palette entity)))
        (inv (inv-bind-pose (skeleton (asset entity)))))
    (setf (palette entity) palette)
    (dotimes (i (length palette))
      (nm* (svref palette i) (svref inv i)))))

(defmethod trial:handle ((ev trial:tick) (entity entity))
  (when (pose entity)
    (update entity (trial:dt ev))
    (update-palette entity)))

(defmethod trial:render ((entity entity) (program trial:shader-program))
  (declare (optimize speed))
  (setf (trial:uniform program "pose") (palette entity))
  (setf (trial:uniform program "model_matrix") (trial:model-matrix))
  (setf (trial:uniform program "view_matrix") (trial:view-matrix))
  (setf (trial:uniform program "projection_matrix") (trial:projection-matrix))
  (setf (trial:uniform program "camera_pos") (trial:location (trial:unit :camera (trial:scene trial:+main+))))
  ;;(gl:bind-texture :texture-2d (trial:gl-name (trial:texture entity)))
  (let* ((vao (trial:vertex-array entity))
         (size (trial:size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (trial:gl-name vao))
    (if (trial::indexed-p vao)
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

uniform mat4 pose[100];

out vec3 normal;
out vec4 world_pos;
out vec2 texcoord;

void main(){
  ivec4 j = ivec4(joints);
  mat4 skin_matrix = (pose[j.x] * weights.x)
                   + (pose[j.y] * weights.y)
                   + (pose[j.z] * weights.z)
                   + (pose[j.w] * weights.w);
  world_pos = model_matrix * skin_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * skin_matrix * vec4(in_normal, 0.0f));
  texcoord = in_texcoord;
  gl_Position = projection_matrix * view_matrix * world_pos;
}")

(trial:define-class-shader (entity :fragment-shader)
  "
uniform sampler2D tex_image;
uniform vec3 camera_pos;

in vec3 normal;
in vec4 world_pos;
in vec2 texcoord;
out vec4 color;

vec3 shade_pointlight(vec3 light_pos, vec3 fragment_pos, vec3 normal){
  vec3 light_dir = normalize(light_pos - fragment_pos);
  vec3 view_dir = normalize(camera_pos - fragment_pos);
  vec3 reflect_dir = reflect(-light_dir, normal);
  float distance = length(light_pos - fragment_pos);
  float attenuation = 1.0 / (1.0 + 0.014 * distance + 0.0007 * distance * distance);

  vec3 ambient = vec3(0.8, 0.8, 0.8) * 0.1;
  vec3 diffuse = vec3(0.8, 0.8, 0.8) * max(dot(normal, light_dir), 0);
  vec3 specular = vec3(0.1, 0.1, 0.1) * pow(max(dot(view_dir, reflect_dir), 0.0), 32);
  
  return attenuation * (ambient+diffuse+specular);
}

void main(){
  color = vec4(shade_pointlight(vec3(10, 10, 10), vec3(world_pos), normal), 1);
}")
