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
                     :context '(:vsync T :version (4 3))))

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

(define-asset (workbench grid) mesh
    (make-line-grid-mesh 10 10 10))

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
    (issue (scene +main+) 'tick :tt 1.0d0 :dt 0.01 :fc 1))
  (when (string= text "b")
    (trial::emit (unit :emitter (scene +main+)) 100))
  (when (string= text "c")
    (clear (unit :emitter (scene +main+)))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'display-controller) scene)
    (enter (make-instance 'trial::sorted-particle-emitter
                          :name :emitter :max-particles 1000000 :particle-rate 300
                          :texture (assets:// :circle-05)
                          :vertex-array (// 'trial 'unit-point)
                          :particle-options `(:velocity -5.0 :randomness 0.0 :size 0.1 :scaling 1.0
                                              :lifespan 1.0 :lifespan-randomness 0.0)) scene)
    (observe! (let ((emitter (unit :emitter (scene +main+))))
                (with-buffer-tx (struct (slot-value emitter 'trial::particle-counter-buffer) :update :read)
                  (slot-value struct 'trial::alive-count)))
              :title "Alive Particles")
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
    ;; Need a standard render pass here because we need the standard-environment-information.
    (let ((render (make-instance 'pbr-render-pass))
          (map (make-instance 'ward)))
      (when (typep (unit :emitter scene) 'trial::depth-colliding-particle-emitter)
        (connect render (unit :emitter scene) scene))
      (connect (port render 'color) (port map 'previous-pass) scene)))
  (maybe-reload-scene))
