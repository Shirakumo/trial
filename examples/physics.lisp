(in-package #:org.shirakumo.fraf.trial.examples)

(define-material (none pbr-material)
  :albedo-texture (// 'trial 'white)
  :metal-rough-occlusion-texture (// 'trial 'white)
  :albedo-factor (vec 1 1 1 1)
  :roughness-factor 1.0
  :metalness-factor 0.0
  :occlusion-factor 0.0)

(define-material (red pbr-material)
  :albedo-texture (// 'trial 'white)
  :metal-rough-occlusion-texture (// 'trial 'white)
  :albedo-factor (vec 1 0 0 1)
  :roughness-factor 0.2
  :metalness-factor 0.0
  :occlusion-factor 0.0)

(define-asset (examples wall) mesh
    (make-cube-mesh '(20 1 20)))

(define-shader-entity physics-sphere (single-material-renderable rigidbody)
  ((vertex-array :initform (// 'trial 'unit-sphere))
   (material :initform (material 'red)))
  (:default-initargs :mass 10.0 :physics-primitives (make-sphere :radius 1.0)))

(defmethod integrate :after ((sphere physics-sphere) dt)
  (unless (v<= (vabs (location sphere)) 200)
    (leave sphere T)))

(define-shader-entity physics-wall (single-material-renderable rigidbody)
  ((vertex-array :initform (// 'examples 'wall))
   (material :initform (material 'none)))
  (:default-initargs :physics-primitives (make-box :bsize (vec 10 0.5 10))))

(define-shader-entity physics-player (single-material-renderable rigidbody listener)
  ((vertex-array :initform (// 'trial 'unit-cube))
   (material :initform (material 'red)))
  (:default-initargs :mass 1000.0 :physics-primitives (make-box :bsize (vec3 0.5))))

(define-handler (physics-player tick) (dt)
  (let ((vel (velocity physics-player))
        (rot (orientation physics-player))
        (spd (* dt 10.0)))
    (when (retained :w) (incf (vz vel) (- spd)))
    (when (retained :s) (incf (vz vel) (+ spd)))
    (when (retained :a) (incf (vx vel) (- spd)))
    (when (retained :d) (incf (vx vel) (+ spd)))
    (when (retained :q) (nq* rot (qfrom-angle +vy+ (+ (* dt)))))
    (when (retained :e) (nq* rot (qfrom-angle +vy+ (- (* dt)))))))

(define-handler (physics-player key-press) (key)
  (case key
    (:space
     (enter (make-instance 'physics-sphere) (container physics-player)))))

(defmethod awake-p ((player physics-player)) T)

(define-example physics
  :title "3D Physics System"
  :description "Playground showcasing the 3D physics simulation features."
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'accelerated-rigidbody-system :units-per-metre 0.1)))
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'target-camera :location (VEC3 0.0 13.5 15) :target (vec 0 0 0) :fov 50) scene)
  (enter (make-instance 'directional-light :direction -vy3+) scene)
  (enter (make-instance 'ambient-light :color (vec3 0.2)) scene)
  (enter (make-instance 'gravity :gravity (vec 0 -10 0)) scene)
  (flet ((wall (location orientation)
           (enter (make-instance 'physics-wall :location location :orientation orientation) scene)))
    (wall (vec 0 0 0) (quat))
    (wall (vec 0 -5 +10) (qfrom-angle +vx+ (/ PI 2)))
    (wall (vec 0 -5 -10) (qfrom-angle +vx+ (/ PI 2)))
    (wall (vec -10 -5 0) (qfrom-angle +vz+ (/ PI 2)))
    (wall (vec +10 -5 0) (qfrom-angle +vz+ (/ PI 2))))
  
  (loop for y from 2.1 to 2.1 by 2.1
        do (loop for x from -8.5 to +9 by 2.1
                 do (loop for z from -8.5 to +0.0 by 2.1
                          for sphere = (make-instance 'physics-sphere :location (vec x y z))
                          do (enter sphere scene))))

  (let ((player (make-instance 'physics-player :name :player :location (vec 0 1 +5))))
    (enter player scene)
    (observe! (location player) :title "Location")
    (observe! (velocity player) :title "Velocity")
    (observe! (rotation player) :title "Rotation")
    (observe! (awake-count (physics-system scene)) :title "Awake #"))
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'ward)))
    (connect (port render 'color) (port map 'previous-pass) scene)))

