(defpackage #:workbench
  (:nicknames #:trial-workbench #:org.shirakumo.fraf.trial.workbench)
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:v #:org.shirakumo.verbose))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass movement (directional-action)
  ())

(defmethod active-p ((action movement)) T)

(load-mapping '((directional movement
                 (point)
                 (stick :one-of ((:l-h :l-v) (:r-h :r-v) (:dpad-h :dpad-v)))
                 (keys :one-of ((:w :a :s :d) (:i :j :k :l)))
                 (buttons :one-of ((:y :x :a :b))))))

(defclass workbench (main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)
                     :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(defmethod update ((main workbench) tt dt fc)
  (if (paused-p main)
      (handle (make-event 'tick :tt tt :dt dt :fc fc) (camera (scene main)))
      (issue (scene main) 'tick :tt tt :dt dt :fc fc))
  (process (scene main)))

(define-pool workbench)

(define-asset (workbench garbage) org.shirakumo.fraf.trial.gltf:asset
    #p "~/large_garbage_bin.glb")

(define-asset (workbench grid) mesh
    (make-line-grid-mesh 10 100 100))

(define-shader-entity ground (trial::single-material-renderable transformed-entity)
  ()
  (:default-initarg :asset (asset 'workbench 'garbage)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'display-controller) scene)
    (observe! "
[P] Pause
[R] Reset
[S] Step
[Ctrl] Look
[WASD] Move
[Space] Ascend
[C] Descend" :title "Controls")
    (let ((physics (make-instance 'rigidbody-system :units-per-metre 1.0))
          (floor (make-instance 'rigidbody :physics-primitives (trial::make-half-space :material :wood))))
      (enter floor physics)
      (loop for i from 0 below 10
            for cube = (make-instance 'cube :location (vec 0 (+ 5 (* i 11)) 0))
            do (enter cube physics)
               (enter cube scene))
      (enter (make-instance 'gravity :gravity (vec 0 -200 0)) physics)
      (enter physics scene))
    (enter (make-instance 'ground :location (vec 0 -50 0)) scene)
    (enter (make-instance 'ground :location (vec 0 +150 0)) scene)
    (enter (make-instance 'ground :location (vec +100 +50 0)) scene)
    (enter (make-instance 'ground :location (vec -100 +50 0)) scene)
    (enter (make-instance 'ground :location (vec 0 +50 +100)) scene)
    (enter (make-instance 'ground :location (vec 0 +50 -100)) scene)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'editor-camera :rotation (vec 0 (* 1.5 PI) 0) :location (vec 75 30 0)) scene)
    (enter (make-instance 'trial::ambient-light :color (vec .1 .1 .1)) scene)
    (enter (make-instance 'trial::point-light :cast-shadows-p T :color (vec 1000 1000 1000) :location (vec 0 50 25)
                                              :linear-attenuation 0.2 :quadratic-attenuation 1.0) scene)
    ;;(enter (make-instance 'trial::directional-light :color (vec 1 1 1)) scene)
    (enter (make-instance 'trial::phong-render-pass) scene))
  (maybe-reload-scene))

