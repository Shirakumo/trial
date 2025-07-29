(in-package #:org.shirakumo.fraf.trial)

(defclass base-animated-entity (mesh-entity)
  ((animation-controller :initform (make-instance 'animation-controller) :accessor animation-controller)))

(define-transfer base-animated-entity animation-controller)

(define-accessor-wrapper-methods clip (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods skeleton (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette-texture (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette-type (base-animated-entity (animation-controller base-animated-entity)))

(defmethod (setf mesh) :after ((meshes cons) (entity base-animated-entity))
  (unless (eq (animation-controller entity) entity)
    (setf (mesh (animation-controller entity)) meshes)))

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
  (list-clips (animation-controller entity)))

(defmethod bind-palette ((pass shader-pass) (entity base-animated-entity))
  (bind (palette-texture entity) :texture5))

(defmethod bind-palette ((pass shader-pass) (texture texture))
  (bind texture :texture5))

(define-shader-entity armature (base-animated-entity lines)
  ((color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(define-handler ((entity armature) (ev tick) :after) ()
  (replace-vertex-data entity (target (animation-controller entity)) :default-color (color entity)))

(define-shader-entity morphed-entity (base-animated-entity listener)
  ((morphs :initform #() :accessor morphs))
  (:shader-file (trial "renderer/morph.glsl")))

(define-transfer morphed-entity morphs)

(defmethod (setf mesh) :after ((meshes cons) (entity morphed-entity))
  (let ((morphs ()))
    (dolist (mesh meshes)
      (when (morphed-p mesh)
        (let ((morph (find-morph mesh (animation-controller entity) NIL)))
          (push (when morph (cons morph (find-morph mesh morph)))
                morphs))))
    (setf (morphs entity) (if morphs (coerce (nreverse morphs) 'vector) #()))))

(defmethod (setf mesh) :after ((mesh animated-mesh) (entity morphed-entity))
  (let ((morph (find-morph mesh (animation-controller entity) NIL)))
    (setf (morphs entity) (if morph (vector (cons morph (find-morph mesh morph))) #()))))

(defmethod stage :after ((entity morphed-entity) (area staging-area))
  (stage (// 'trial :texture-1d-array) area)
  (loop for (morph) across (morphs entity)
        do (when morph (stage morph area))))

(defmethod morphed-p ((entity morphed-entity))
  (< 0 (length (morphs entity))))

(defmethod morphed-p ((entity entity)) NIL)

(define-shader-entity skinned-entity (base-animated-entity)
  ((mesh :initform NIL :accessor mesh))
  (:shader-file (trial "renderer/skin-matrix.glsl")))

(defmethod stage :after ((entity skinned-entity) (area staging-area))
  (stage (// 'trial :texture-1d-array) area))

(defmethod render-with :before ((pass shader-pass) (entity skinned-entity) (program shader-program))
  ;; TODO: only compute the actual palette texture once it is needed in the render.
  ;;       otherwise it is computed every physics tick, which may be too often or too
  ;;       infrequent. Regardless, to do this we need access to the frame counter during
  ;;       rendering, for which there is no provision right now.
  (setf (uniform program "pose") (if (palette-texture entity)
                                     (bind-palette pass entity)
                                     (bind-palette pass (// 'trial :texture-1d-array)))))

(defmethod skinned-p ((entity skinned-entity))
  (skeleton entity))

(defmethod skinned-p ((entity entity)) NIL)

(define-handler (skinned-entity tick :after) (dt tt fc)
  (unless (eq skinned-entity (animation-controller skinned-entity))
    (update (animation-controller skinned-entity) tt dt fc)))

(define-shader-entity quat2-skinned-entity (skinned-entity)
  ((animation-controller :initform (make-instance 'animation-controller :palette-type 'quat2)))
  (:shader-file (trial "renderer/skin-dquat.glsl"))
  (:inhibit-shaders (skinned-entity :vertex-shader)))

(defmethod (setf animation-controller) :after ((controller animation-controller) (entity quat2-skinned-entity))
  (setf (palette-type controller) 'quat2))

(define-shader-entity animated-entity (skinned-entity morphed-entity)
  ())

(defmethod render-with :before ((pass shader-pass) (entity animated-entity) (program shader-program))
  (setf (uniform program "animation")
        (+ (if (morphed-p entity) 1 0)
           (if (skinned-p entity) 2 0))))

(define-class-shader (animated-entity :vertex-shader)
  "layout (location = TRIAL_V_LOCATION) in vec3 in_position;
layout (location = TRIAL_V_NORMAL) in vec3 in_normal;
layout (location = TRIAL_V_UV) in vec2 in_uv;
layout (location = TRIAL_V_JOINTS) in vec4 in_joints;
layout (location = TRIAL_V_WEIGHTS) in vec4 in_weights;
uniform int animation = 0;

out vec3 normal;
out vec4 world_pos;
out vec2 uv;

void main(){
  vec3 position = in_position;
  normal = in_normal;
  uv = in_uv;

  if(0 < (animation & 1)) morph_vertex(position, normal, uv);
  if(0 < (animation & 2)) skin_vertex(position, normal, in_joints, in_weights);

  world_pos = model_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * vec4(normal, 0.0f));
  gl_Position = projection_matrix * view_matrix * world_pos;
}")
