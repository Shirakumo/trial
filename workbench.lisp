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
  ()
  (:default-initargs :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench triangle) mesh
    (with-mesh-construction (v :attributes (location color))
      (v -0.5 -0.5 0.0 1 0 0 1)
      (v +0.5 -0.5 0.0 0 1 0 1)
      (v +0.0 +0.5 0.0 0 0 1 1)
      (finalize-data)))

(define-shader-entity basic-triangle (listener vertex-entity transformed-entity vertex-colored-entity)
  ((vertex-array :initform (// 'workbench 'triangle))
   (transform :initform (transform))))

(define-handler (basic-triangle tick) (dt)
  (vsetf (location basic-triangle) (* 0.5 (width *context*)) (* -0.5 (height *context*)) 0)
  (vsetf (scaling basic-triangle) (* 0.5 (width *context*)) (* 0.5 (height *context*)) 1)
  (trotate (tf basic-triangle) (qfrom-angle +vx+ (* 0.8 dt)))
  (trotate (tf basic-triangle) (qfrom-angle +vy+ (* 1 dt)))
  (trotate (tf basic-triangle) (qfrom-angle +vz+ (* 1.3 dt))))

(define-shader-entity bouncer (listener vertex-entity located-entity textured-entity)
  ((texture :initform (// 'trial 'trial::cat))
   (vertex-array :initform (// 'trial 'unit-square))
   (velocity :initform (vec 1 1 0) :accessor velocity)))

(defmethod apply-transforms progn ((bouncer bouncer))
  (scale-by 100 100 1))

(define-handler (bouncer tick) (dt)
  (let ((loc (location bouncer))
        (vel (velocity bouncer)))
    (nv+* loc vel (* 100 dt))
    (when (< (vx loc) 40)
      (setf (vx loc) 40)
      (setf (vx vel) (- (vx vel))))
    (when (< (- 1280 60) (vx loc))
      (setf (vx loc) (- 1280 60))
      (setf (vx vel) (- (vx vel))))
    (when (< -30 (vy loc))
      (setf (vy loc) -30)
      (setf (vy vel) (- (vy vel))))
    (when (< (vy loc) (- (- 720 70)))
      (setf (vy loc) (- (- 720 70)))
      (setf (vy vel) (- (vy vel))))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (disable-feature :cull-face)
    (!meye (view-matrix))
    (nmortho (projection-matrix) -10 +1270 -700 20 0.1 1)
    (enter (make-instance 'basic-triangle) scene)
    (enter (make-instance 'bouncer) scene)
    (enter (make-instance 'repl :foreground (vec 1 1 1 1)) scene)
    (enter (make-instance 'system-stats) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
