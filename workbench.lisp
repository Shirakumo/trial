(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench particles) mesh
    (make-particle-storage (make-cube 5) :vertex-attributes '(location))
  :data-usage :stream-draw)

(define-asset (workbench grid) mesh
    (make-line-grid 10 200 200))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)))

(define-shader-subject fireworks (particle-emitter)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'particles)))

(defmethod initial-particle-state ((fireworks fireworks) tick loc vel)
  (vsetf loc 0 0 0)
  (vsetf vel (- (random 1.0) 0.5) 3.0 (- (random 1.0) 0.5))
  (nv* vel 2)
  (+ 1.0 (random 1.0)))

(defmethod update-particle-state ((fireworks fireworks) tick loc vel life)
  (nv+ loc vel)
  ;;(decf (vy3 vel) 0.1)
  (- life (dt tick)))

(defmethod new-particle-count ((fireworks fireworks) tick)
  (if (= 0 (mod (fc tick) 60))
      100000
      0))

(defmethod paint :before ((fireworks fireworks) (pass shader-pass))
  (let ((program (shader-program-for-pass pass fireworks)))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "model_matrix") (model-matrix))))

(define-class-shader (fireworks :vertex-shader)
  "layout (location = 0) in vec3 vtx_location;
layout (location = 1) in vec3 location;
layout (location = 2) in vec3 velocity;
layout (location = 3) in float lifetime;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

out PARTICLE_DATA{
  vec3 location;
  vec3 velocity;
  float lifetime;
} particle_out;

void main(){
  vec3 position = vtx_location + location;
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);

  particle_out.location = location;
  particle_out.velocity = velocity;
  particle_out.lifetime = lifetime;
}")

(define-class-shader (fireworks :fragment-shader)
  "out vec4 color;

in PARTICLE_DATA{
  vec3 location;
  vec3 velocity;
  float lifetime;
} particle;

void main(){
  color = vec4(clamp(particle.lifetime, 0, 1));
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'fireworks) scene)
    (enter (make-instance 'editor-camera :location (vec 0 100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
