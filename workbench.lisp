(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
    (make-line-grid 10 200 200))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)))

(define-shader-subject fireworks (particle-emitter)
  ()
  (:default-initargs :name :fireworks
                     :vertex-array (make-particle-storage (make-sphere 2)
                                                          :max-particles 16384
                                                          :vertex-attributes '(location))))

(defmethod initial-particle-state ((fireworks fireworks) tick loc vel life)
  (vsetf loc 0 0 0)
  (flet ((hash (x)
           (sxhash x)))
    (let ((dir (polar->cartesian (vec2 (/ (hash (fc tick)) (ash 2 60)) (mod (hash (fc tick)) 100)))))
      (vsetf vel (vx dir) (+ 2.5 (mod (hash (fc tick)) 2)) (vy dir))))
  (vsetf life 0 (+ 3.0 (random 1.0))))

(defmethod update-particle-state ((fireworks fireworks) tick loc vel life)
  (nv+ loc vel)
  (decf (vy3 vel) 0.005)
  (when (< (vy loc) 0)
    (setf (vy vel) (- (vy vel)))
    (setf (vy loc) 0))
  (when (< (abs (- (vx life) 2.5)) 0.05)
    (let ((dir (polar->cartesian (vec3 (+ 1.5 (random 0.125)) (random (* 2 PI)) (random (* 2 PI))))))
      (vsetf vel (vx dir) (vy dir) (vz dir))))
  (incf (vx2 life) (dt tick)))

(defmethod new-particle-count ((fireworks fireworks) tick)
  (if (= 0 (mod (fc tick) (* 10 1)))
      128 0))

(defmethod paint :before ((fireworks fireworks) (pass shader-pass))
  (let ((program (shader-program-for-pass pass fireworks)))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "model_matrix") (model-matrix))))

(define-class-shader (fireworks :vertex-shader)
  "layout (location = 0) in vec3 vtx_location;
layout (location = 1) in vec2 lifetime;
layout (location = 2) in vec3 location;
layout (location = 3) in vec3 velocity;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

out PARTICLE_DATA{
  vec2 lifetime;
  vec3 location;
  vec3 velocity;
} particle_out;

void main(){
  vec3 position = vtx_location + location;
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);

  particle_out.lifetime = lifetime;
  particle_out.location = location;
  particle_out.velocity = velocity;
}")

(define-class-shader (fireworks :fragment-shader)
  "out vec4 color;

in PARTICLE_DATA{
  vec2 lifetime;
  vec3 location;
  vec3 velocity;
} particle;

void main(){
  if(particle.lifetime.x <= 2.5)
    color = vec4(1);
  else{
    float lt = particle.lifetime.y-particle.lifetime.x;
    color = vec4(lt*2, lt, 0, 1);
  }
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'fireworks) scene)
    (enter (make-instance 'editor-camera :location (vec 0 100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
