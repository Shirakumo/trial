(defpackage #:workbench
  (:nicknames #:trial-workbench #:org.shirakumo.fraf.trial.workbench)
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:v #:org.shirakumo.verbose))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass workbench (main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :clear-color (vec 0.1 0.1 0.1)
                     :context '(:vsync T)))

(defmethod update ((main workbench) tt dt fc)
  (if (paused-p main)
      (handle (make-event 'tick :tt tt :dt dt :fc fc) (camera (scene main)))
      (issue (scene main) 'tick :tt tt :dt dt :fc fc))
  (process (scene main)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench sphere) mesh
    (make-sphere-mesh 0.3 :segments 16))

(define-asset (workbench cube) mesh
    (make-cube-mesh 0.6))

(define-asset (workbench plane) mesh
    (make-rectangle-mesh 10 10))

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

(define-shader-entity meshy (mesh-entity single-material-renderable transformed-entity)
  ())

(define-shader-entity planey (single-material-renderable transformed-entity)
  ((vertex-array :initform (// 'workbench 'plane))
   (material :initform (material 'none))))

(define-shader-entity spherey (single-material-renderable rigidbody)
  ((vertex-array :initform (// 'workbench 'sphere))
   (material :initform (material 'red)))
  (:default-initargs :mass 10.0 :physics-primitives (make-sphere :radius 0.3 :material :glass)))

(define-handler (controller text-entered) (text)
  (when (string= text "p")
    (setf (paused-p +main+) (not (paused-p +main+))))
  (when (string= text "s")
    (issue (scene +main+) 'tick :tt 1.0d0 :dt 0.01 :fc 1)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
    
    (enter (make-instance 'skybox :texture (assets:// :sandy-beach :environment-map)) scene)
    (enter (make-instance 'planey :location (vec 0 5 -5)) scene)
    (enter (make-instance 'planey :orientation (qfrom-angle +vx+ (deg->rad -90))) scene)
    (enter (make-instance 'meshy :asset (assets:asset :marble-bust) :scaling (vec 10 10 10)) scene)

    (let ((physics (make-instance 'rigidbody-system :units-per-metre 0.1)))
      (enter (make-instance 'rigidbody :physics-primitives (make-box :bsize (vec 5 1 5) :location (vec 0 -1 0) :material :ice)) physics)
      (enter (make-instance 'rigidbody :physics-primitives (make-box :bsize (vec 5 5 1) :location (vec 0 5 -6) :material :ice)) physics)
      (loop for i from 0 below 10
            for cube = (make-instance 'spherey :location (vec (+ 3 (random 0.1)) (+ 5 (* i 1)) (random 0.1)))
            do (enter cube physics)
               (enter cube scene))
      (enter (make-instance 'gravity :gravity (vec 0 -10 0)) physics)
      (enter physics scene))

    (enter (make-instance 'environment-light :asset (assets:asset :sandy-beach) :color (vec 0.3 0.3 0.3)) scene)
    (enter (make-instance 'point-light :location (vec 2.0 4.0 -1.0) :color (vec 15.0 0 0) :cast-shadows-p T) scene)
    (enter (make-instance 'spot-light :location (vec -5.0 4.0 0.0) :color (vec 0 15.0 0)
                                      :inner-radius 10 :outer-radius 20 :direction (vec 2 -1 0)
                                      :cast-shadows-p T) scene)
    (enter (make-instance 'directional-light  :color (vec 0 0 15.0)
                                              :direction (vec 0 -1 1)
                                              :cast-shadows-p T) scene)

    (let ((zpre (make-instance 'trial::z-prepass))
          (render (make-instance 'trial::ssr-pbr-render-pass :name :render))
          (map (make-instance 'tone-mapping-pass :name :map)))
      (connect (port zpre 'depth) (port render 'depth-map) scene)
      (connect (port render 'color) (port map 'previous-pass) scene)))
  (maybe-reload-scene))
