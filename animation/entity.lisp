#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
        (strength (clamp 0.0 (float strength 0f0) 1.0)))
    (sample-pose clip (animation-layer-pose layer) (+ (start-time clip) (* strength (duration clip))))
    (setf (animation-layer-strength layer) strength)))

(defclass layer-controller ()
  ((layers :initform (make-hash-table :test 'equalp) :accessor layers)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod update ((controller layer-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (loop for layer being the hash-values of (layers controller)
        do (layer-onto (pose controller) (pose controller) (animation-layer-pose layer) (animation-layer-base layer))))

(defmethod add-layer ((layer animation-layer) (controller layer-controller) &key name)
  (setf (gethash name (layers controller)) layer))

(defmethod add-layer ((clip clip) (controller layer-controller) &key (strength 0.0) (name (name clip)))
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

(defmethod update ((controller fade-controller) tt dt fc)
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

(define-shader-entity armature (fade-controller lines listener)
  ((animation-asset :initarg :asset :accessor animation-asset)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(defmethod initialize-instance :after ((entity armature) &key asset)
  (register-generation-observer entity asset))

(defmethod stage :after ((entity armature) (area staging-area))
  (stage (animation-asset entity) area))

(defmethod observe-generation ((entity armature) (asset animation-asset) res)
  (setf (skeleton entity) (skeleton asset))
  (typecase (clip entity)
    (string (play (gethash (clip entity) (clips asset)) entity))
    ((eql T) (play (loop for v being the hash-values of (clips asset) return v) entity))
    (null (setf (pose entity) (rest-pose* (skeleton asset))))))

(defmethod handle ((ev tick) (entity armature))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))
    (replace-vertex-data entity (pose entity) :default-color (color entity))))

(define-shader-entity animated-entity (fade-controller layer-controller transformed-entity vertex-entity listener)
  ((palette :initform #() :accessor palette)
   (mesh :initarg :mesh :initform NIL :accessor mesh)
   (animation-asset :initarg :asset :accessor animation-asset)))

(defmethod initialize-instance :after ((entity animated-entity) &key)
  (register-generation-observer entity (animation-asset entity)))

(defmethod stage :after ((entity animated-entity) (area staging-area))
  ;; FIXME: fuck. don't know how to load the resources?
  (stage (animation-asset entity) area))

(defmethod observe-generation ((entity animated-entity) (asset animation-asset) res)
  (setf (animation-asset entity) asset))

(defmethod (setf animation-asset) :after ((asset animation-asset) (entity animated-entity))
  (setf (mesh entity) (or (mesh entity) T))
  (if (skeleton asset)
      (setf (skeleton entity) (skeleton asset))
      (setf (palette entity) #(#.(meye 4))))
  (play (or (clip entity) T) entity))

(defmethod fade-to ((name string) (entity animated-entity) &rest args)
  (let ((clip (gethash name (clips (animation-asset entity)))))
    (if clip
        (apply #'fade-to clip entity args)
        #-trial-release
        (error "No animation clip named ~s found." name))))

(defmethod play ((name string) (entity animated-entity))
  (let ((clip (gethash name (clips (animation-asset entity)))))
    (if clip
        (play clip entity)
        #-trial-release
        (error "No animation clip named ~s found." name))))

(defmethod play ((anything (eql T)) (entity animated-entity))
  (loop for clip being the hash-values of (clips (animation-asset entity))
        do (return (play clip entity))))

(defmethod (setf pose) :after ((pose pose) (entity animated-entity))
  (update-palette entity))

(defmethod (setf mesh) :after ((mesh skinned-mesh) (entity animated-entity))
  (setf (vertex-array entity) (resource (animation-asset entity) (name mesh))))

(defmethod (setf mesh) ((name string) (entity animated-entity))
  (let ((mesh (gethash name (meshes (animation-asset entity)))))
    (if mesh
        (setf (mesh entity) mesh)
        #-trial-release
        (error "No mesh named ~s found." name))))

(defmethod (setf mesh) ((anything (eql T)) (entity animated-entity))
  (loop for mesh being the hash-values of (meshes (animation-asset entity))
        do (return (setf (mesh entity) mesh))))

(defun update-palette (entity)
  (let ((palette (matrix-palette (pose entity) (palette entity)))
        (inv (inv-bind-pose (skeleton (animation-asset entity)))))
    (setf (palette entity) palette)
    (dotimes (i (length palette))
      (nm* (svref palette i) (svref inv i)))))

(defmethod handle ((ev tick) (entity animated-entity))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))
    (update-palette entity)))

(defmethod render :before ((entity animated-entity) (program shader-program))
  (declare (optimize speed))
  (setf (uniform program "pose") (palette entity)))

(define-class-shader (animated-entity :vertex-shader)
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
