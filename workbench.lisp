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
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)
                     :context '(:vsync T)))

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
  (let ((strength (* dt 10000)))
    (when (retained :a)
      (nv+ (trial::torque cube) (vec strength 0 0)))
    (when (retained :d)
      (nv- (trial::torque cube) (vec strength 0 0)))
    (when (retained :w)
      (nv+ (trial::torque cube) (vec 0 strength 0)))
    (when (retained :s)
      (nv- (trial::torque cube) (vec 0 strength 0)))))

(define-handler (cube text-entered) (text)
  (when (string= text "r")
    (vsetf (location cube) 0 10 0)
    (vsetf (trial::velocity cube) 0 0 0)
    (vsetf (trial::rotation cube) 0 0 0)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (let ((physics (make-instance 'trial::rigidbody-system))
          (floor (make-instance 'trial::rigidbody :physics-primitives (trial::make-half-space)))
          (cube (make-instance 'cube :location (vec 0 10 0) :orientation (q* (qfrom-angle +vz+ (/ PI 4))
                                                                             (qfrom-angle +vx+ (/ PI 4))))))
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

