(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames)
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

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench cat) image
    #p"cat.png")

(define-asset (workbench cube) mesh
    (make-cube-mesh 10))

(define-asset (workbench grid) mesh
    (make-line-grid-mesh 10 100 100))

(define-shader-entity cube (vertex-entity textured-entity trial::rigidbody listener)
  ((name :initform 'cube)
   (texture :initform (// 'workbench 'cat))
   (vertex-array :initform (// 'workbench 'cube)))
  (:default-initargsp
   :mass 10.0
   :physics-primitives (trial::make-box :bsize (vec 5 5 5))))

(define-handler (cube tick) (dt)
  (when (retained :a)
    (nv+ (trial::rotation cube) (vec dt 0 0))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (let ((physics (make-instance 'trial::rigidbody-system))
          (floor (make-instance 'trial::rigidbody :physics-primitives (trial::make-half-space)))
          (cube (make-instance 'cube :location (vec 0 50 0))))
      (enter (make-instance 'trial::gravity) physics)
      (enter cube physics)
      (enter floor physics)
      (enter cube scene)
      (enter physics scene))
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'target-camera :location (vec 0 100 100)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))

