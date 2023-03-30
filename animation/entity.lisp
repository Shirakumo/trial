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
    (sample (animation-layer-pose layer) clip (+ (start-time clip) (* strength (duration clip))))
    (setf (animation-layer-strength layer) strength)))

(defclass layer-controller ()
  ((layers :initform (make-hash-table :test 'equalp) :accessor layers)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod shared-initialize :after ((controller layer-controller) slots &key layers)
  (loop for layer in layers
        for (clip . args) = (enlist layer)
        do (apply #'add-layer clip controller args)))

(defmethod describe-object :after ((controller layer-controller) stream)
  (terpri stream)
  (format stream "Layers:~%")
  (let ((layers (sort (alexandria:hash-table-keys (layers controller)) #'string<)))
    (if layers
        (loop for name in layers
              for layer = (layer name controller)
              do (format stream "  ~3d% ~s~%" (round (* 100 (animation-layer-strength layer))) name))
        (format stream "  No layers.~%"))))

(defmethod update ((controller layer-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (loop for layer being the hash-values of (layers controller)
        do (layer-onto (pose controller) (pose controller) (animation-layer-pose layer) (animation-layer-base layer))))

(defmethod add-layer ((layer animation-layer) (controller layer-controller) &key name)
  (setf (layer name controller) layer))

(defmethod add-layer ((clip clip) (controller layer-controller) &key (strength 0.0) (name (name clip)))
  (setf (layer name controller) (make-animation-layer clip (skeleton controller) :strength strength)))

(defmethod remove-layer (name (controller layer-controller))
  (setf (layer name controller) NIL))

(defmethod layer (name (controller layer-controller))
  (gethash name (layers controller)))

(defmethod (setf layer) ((layer animation-layer) name (controller layer-controller))
  (setf (gethash name (layers controller)) layer))

(defmethod (setf layer) ((null null) name (controller layer-controller))
  (remhash name (layers controller))
  null)

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

(defmethod describe-object :after ((controller fade-controller) stream)
  (terpri stream)
  (format stream "Current Clip:~%")
  (if (clip controller)
      (format stream "  ~4f / ~4f ~s~%"
              (clock controller) (duration (clip controller)) (name (clip controller)))
      (format stream "  No current clip.~%"))
  (terpri stream)
  (format stream "Fade Targets:~%")
  (if (< 0 (length (targets controller)))
      (loop for target across (targets controller)
            do (format stream "  ~4f / ~4f ~s~%"
                       (fade-target-clock target) (fade-target-duration target)
                       (name (fade-target-clip target))))
      (format stream "  No current fade targets.~%")))

(defmethod play ((target clip) (controller fade-controller))
  (setf (fill-pointer (targets controller)) 0)
  (setf (clip controller) target)
  (pose<- (pose controller) (rest-pose (skeleton controller)))
  (setf (clock controller) (start-time target))
  (sample (pose controller) (clip controller) (clock controller)))

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
      (let ((time (sample (pose controller) (clip controller) (+ (clock controller) dt))))
        (setf (clock controller) time)
        (loop for target across targets
              do (setf (fade-target-clock target) (sample (fade-target-pose target) (fade-target-clip target) (+ (fade-target-clock target) dt)))
                 (incf (fade-target-elapsed target) dt)
                 (let ((time (min 1.0 (/ (fade-target-elapsed target) (fade-target-duration target)))))
                   (blend-into (pose controller) (pose controller) (fade-target-pose target) time)))))))

(define-shader-entity base-animated-entity (ik-controller layer-controller fade-controller listener)
  ((animation-asset :initarg :asset :initform NIL :accessor animation-asset)))

(defmethod initialize-instance :after ((entity base-animated-entity) &key asset)
  (when asset
    (register-generation-observer entity asset)))

(defmethod describe-object :after ((entity base-animated-entity) stream)
  (terpri stream)
  (format stream "Available Clips:~%")
  (if (list-clips entity)
      (loop for clip in (list-clips entity)
            do (format stream "  ~s~%" clip))
      (format stream "  No currently available clips.~%"))
  (terpri stream)
  (format stream "Skeleton:~%")
  (describe-skeleton (skeleton entity) stream))

(defmethod stage :after ((entity base-animated-entity) (area staging-area))
  (stage (animation-asset entity) area))

(defmethod observe-generation ((entity base-animated-entity) (asset animation-asset) res)
  (setf (animation-asset entity) asset))

(defmethod (setf animation-asset) :after ((asset animation-asset) (entity base-animated-entity))
  (when (skeleton asset)
    (setf (skeleton entity) (skeleton asset)))
  (play (or (clip entity) T) entity))

(defmethod find-clip (name (entity base-animated-entity) &optional (errorp T))
  (if (null (animation-asset entity))
      (when errorp (error "No such clip ~s found on ~a" name entity))
      (find-clip name (animation-asset entity) errorp)))

(defmethod list-clips ((entity base-animated-entity))
  (when (animation-asset entity)
    (list-clips (animation-asset entity))))

(defmethod add-layer (clip-name (entity base-animated-entity) &key (name NIL name-p))
  (let ((clip (find-clip clip-name entity)))
    (add-layer clip entity :name (if name-p name (name clip)))))

(defmethod fade-to ((name string) (entity base-animated-entity) &rest args)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod play ((name string) (entity base-animated-entity))
  (play (find-clip name entity) entity))

(defmethod play ((anything (eql T)) (entity base-animated-entity))
  (loop for clip being the hash-values of (clips (animation-asset entity))
        do (return (play clip entity))))

(defmethod handle ((ev tick) (entity base-animated-entity))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))))

(define-shader-entity armature (base-animated-entity lines)
  ((animation-asset :initarg :asset :accessor animation-asset)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(defmethod handle ((ev tick) (entity armature))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))
    (replace-vertex-data entity (pose entity) :default-color (color entity))))

(define-shader-entity animated-entity (base-animated-entity transformed-entity vertex-entity)
  ((palette :initform #() :accessor palette)
   (mesh :initarg :mesh :initform NIL :accessor mesh)))

(defmethod (setf animation-asset) :after ((asset animation-asset) (entity animated-entity))
  (setf (mesh entity) (or (mesh entity) T))
  (unless (skeleton asset)
    (setf (palette entity) #(#.(meye 4)))))

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

(defmethod (setf ik-system) :after ((system ik-system) name (entity animated-entity))
  ;; Hook up our local transform to the IK system's. Since the identity never changes
  ;; the properties "transfer".
  (setf (slot-value system 'transform) (tf entity)))

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
