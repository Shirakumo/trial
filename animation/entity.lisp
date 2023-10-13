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
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)
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
  (unless (eq target (clip controller))
    (setf (playback-speed controller) 1.0)
    (setf (fill-pointer (targets controller)) 0)
    (setf (clip controller) target)
    (pose<- (pose controller) (rest-pose (skeleton controller)))
    (setf (clock controller) (start-time target))
    (sample (pose controller) (clip controller) (clock controller))))

(defmethod fade-to ((target clip) (controller fade-controller) &key (duration 0.2))
  (let ((targets (targets controller)))
    (cond ((null (clip controller))
           (play target controller))
          ((and (or (= 0 (length targets))
                    (not (eq target (fade-target-clip (aref targets (1- (length targets)))))))
                (not (eq target (clip controller))))
           (setf (playback-speed controller) 1.0)
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
      (let ((time (sample (pose controller) (clip controller) (+ (clock controller) (* (playback-speed controller) dt)))))
        (setf (clock controller) time)
        (loop for target across targets
              do (setf (fade-target-clock target) (sample (fade-target-pose target) (fade-target-clip target) (+ (fade-target-clock target) dt)))
                 (incf (fade-target-elapsed target) dt)
                 (let ((time (min 1.0 (/ (fade-target-elapsed target) (fade-target-duration target)))))
                   (blend-into (pose controller) (pose controller) (fade-target-pose target) time)))))))

(define-shader-entity base-animated-entity (mesh-entity ik-controller layer-controller fade-controller listener)
  ())

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

(defmethod (setf mesh-asset) :after ((asset animation-asset) (entity base-animated-entity))
  (when (skeleton asset)
    (setf (skeleton entity) (skeleton asset)))
  (play (or (clip entity) T) entity))

(defmethod find-clip (name (entity base-animated-entity) &optional (errorp T))
  (if (null (mesh-asset entity))
      (when errorp (error "No such clip ~s found on ~a" name entity))
      (find-clip name (mesh-asset entity) errorp)))

(defmethod list-clips ((entity base-animated-entity))
  (when (mesh-asset entity)
    (list-clips (mesh-asset entity))))

(defmethod add-layer (clip-name (entity base-animated-entity) &key (name NIL name-p))
  (let ((clip (find-clip clip-name entity)))
    (add-layer clip entity :name (if name-p name (name clip)))))

(defmethod fade-to ((name string) (entity base-animated-entity) &rest args)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod play ((name string) (entity base-animated-entity))
  (play (find-clip name entity) entity))

(defmethod play ((anything (eql T)) (entity base-animated-entity))
  (loop for clip being the hash-values of (clips (mesh-asset entity))
        do (return (play clip entity))))

(defmethod handle ((ev tick) (entity base-animated-entity))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))))

(define-shader-entity armature (base-animated-entity lines)
  ((color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(defmethod handle ((ev tick) (entity armature))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))
    (replace-vertex-data entity (pose entity) :default-color (color entity))))

(define-shader-entity animated-entity (base-animated-entity transformed-entity)
  ((palette :initform #() :accessor palette)
   (palette-texture :initform (make-instance 'texture :target :texture-1d-array :width 3 :height 1 :internal-format :rgba32f :min-filter :nearest :mag-filter :nearest) :accessor palette-texture)
   (palette-data :initform (make-array 0 :element-type 'single-float) :accessor palette-data)
   (mesh :initarg :mesh :initform NIL :accessor mesh)))

(defmethod stage :after ((entity animated-entity) (area staging-area))
  (stage (palette-texture entity) area))

(defmethod (setf mesh-asset) :after ((asset animation-asset) (entity animated-entity))
  (unless (skeleton asset)
    (setf (palette entity) #(#.(meye 4)))))

(defmethod (setf pose) :after ((pose pose) (entity animated-entity))
  (update-palette entity))

(defmethod (setf ik-system) :after ((system ik-system) name (entity animated-entity))
  ;; Hook up our local transform to the IK system's. Since the identity never changes
  ;; the properties "transfer".
  (setf (slot-value system 'transform) (tf entity)))

(defmethod update-palette ((entity animated-entity))
  (let* ((palette (matrix-palette (pose entity) (palette entity)))
         (texinput (%adjust-array (palette-data entity) (* 12 (length (pose entity))) (constantly 0f0)))
         (texture (palette-texture entity))
         (inv (mat-inv-bind-pose (skeleton (mesh-asset entity)))))
    (dotimes (i (length palette) (setf (palette entity) palette))
      (let ((mat (nm* (svref palette i) (svref inv i))))
        (replace texinput (marr4 mat) :start1 (* i 12) :end2 12)))
    (setf (palette-data entity) texinput)
    (setf (height texture) (length palette))
    (when (gl-name texture)
      (update-buffer-data texture texinput :pixel-type :float :pixel-format :rgba))))

(defmethod handle ((ev tick) (entity animated-entity))
  (when (pose entity)
    (update entity (tt ev) (dt ev) (fc ev))
    (update-palette entity)))

(defmethod render :before ((entity animated-entity) (program shader-program))
  (declare (optimize speed))
  ;; KLUDGE: This is Bad
  (%gl:active-texture :texture5)
  (gl:bind-texture :texture-1d-array (gl-name (palette-texture entity)))
  (setf (uniform program "pose") 5))

(define-class-shader (animated-entity :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 3) in vec4 joints;
layout (location = 4) in vec4 weights;

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

(define-shader-entity quat2-animated-entity (animated-entity)
  ()
  (:inhibit-shaders (animated-entity :vertex-shader)))

(defmethod update-palette ((entity quat2-animated-entity))
  (let ((palette (quat2-palette (pose entity) (palette entity)))
        (inv (quat-inv-bind-pose (skeleton (mesh-asset entity)))))
    (dotimes (i (length palette) (setf (palette entity) palette))
      (nq* (svref palette i) (svref inv i)))))

(define-class-shader (quat2-animated-entity :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 3) in vec4 joints;
layout (location = 4) in vec4 weights;

uniform mat2x4 pose[120];

out vec3 normal;
out vec4 world_pos;
out vec2 uv;

vec4 quat_mul(vec4 q1, vec4 q2){
  return vec4(q2.x*q1.w + q2.y*q1.z - q2.z*q1.y + q2.w*q1.x,
             -q2.x*q1.z + q2.y*q1.w + q2.z*q1.x + q2.w*q1.y,
              q2.x*q1.y - q2.y*q1.x + q2.z*q1.w + q2.w*q1.z,
             -q2.x*q1.x - q2.y*q1.y - q2.z*q1.z + q2.w*q1.w);
}

vec4 dquat_vector(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec3 r_vector = real.xyz;
  float r_scalar = real.w;
  vec3 rotated = r_vector*2*dot(r_vector, v)
    + v*(r_scalar*r_scalar - dot(r_vector, r_vector))
    + cross(r_vector, v)*2*r_scalar;
  return vec4(rotated, 0);
}

vec4 dquat_point(mat2x4 dq, vec3 v){
  vec4 real = dq[0];
  vec4 dual = dq[1];
  vec3 rotated = dquat_vector(dq, v).xyz;
  vec4 conjugate = vec4(-real.xyz, real.w);
  vec3 t = quat_mul(conjugate, dual*2).xyz;
  return vec4(rotated+t, 1);
}

mat2x4 dquat_normalized(mat2x4 dq){
  float inv_mag = 1.0 / length(dq[0]);
  dq[0] *= inv_mag;
  dq[1] *= inv_mag;
  return dq;
}

void main(){
  ivec4 j = ivec4(joints);

  mat2x4 skin_dq = dquat_normalized(
    + (pose[j.x] * weights.x)
    + (pose[j.y] * weights.y)
    + (pose[j.z] * weights.z)
    + (pose[j.w] * weights.w));
  world_pos = model_matrix * dquat_point(skin_dq, position);
  normal = vec3(model_matrix * dquat_vector(skin_dq, in_normal));
  uv = in_uv;
  gl_Position = projection_matrix * view_matrix * world_pos;
}")
