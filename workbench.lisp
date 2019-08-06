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

(define-asset (workbench particles) vertex-struct-buffer
    'simple-particle
  :struct-count 1024)

(define-shader-subject fireworks (simple-particle-emitter)
  ()
  (:default-initargs :particle-mesh (change-class (make-sphere 1) 'vertex-array :vertex-attributes '(location))
                     :particle-buffer (asset 'workbench 'particles)))

(defmethod initial-particle-state ((fireworks fireworks) tick particle)
  (let ((dir (polar->cartesian (vec2 (/ (sxhash (fc tick)) (ash 2 60)) (mod (sxhash (fc tick)) 100)))))
    (setf (velocity particle) (vec (vx dir) (+ 2.5 (mod (sxhash (fc tick)) 2)) (vy dir))))
  (setf (lifetime particle) (vec 0 (+ 3.0 (random 1.0)))))

(defmethod update-particle-state :before ((fireworks fireworks) tick particle output)
  (let ((vel (velocity particle)))
    (decf (vy3 vel) 0.005)
    (when (< (abs (- (vx (lifetime particle)) 2.5)) 0.05)
      (let ((dir (polar->cartesian (vec3 (+ 1.5 (random 0.125)) (random (* 2 PI)) (random (* 2 PI))))))
        (vsetf vel (vx dir) (vy dir) (vz dir))))
    (setf (velocity output) vel)))

(defmethod new-particle-count ((fireworks fireworks) tick)
  (if (= 0 (mod (fc tick) (* 10 1)))
      128 0))

(define-class-shader (fireworks :vertex-shader 1)
  "layout (location = 1) in vec2 in_lifetime;
layout (location = 2) in vec3 location;

out vec2 lifetime;

void main(){
  lifetime = in_lifetime;
}")

(define-class-shader (fireworks :fragment-shader)
  "out vec4 color;

in vec2 lifetime;

void main(){
  if(lifetime.x <= 2.5)
    color = vec4(1);
  else{
    float lt = lifetime.y-lifetime.x;
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
