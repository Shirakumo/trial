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
    (with-mesh-construction (v finalize (location color))
      (v -0.5 -0.5 0.0 1 0 0 1)
      (v +0.5 -0.5 0.0 0 1 0 1)
      (v +0.0 +0.5 0.0 0 0 1 1)
      (finalize-data)))

(define-shader-entity basic-triangle (listener vertex-entity vertex-colored-entity)
  ((vertex-array :initform (// 'workbench 'triangle))))

(defmethod apply-transforms progn ((triangle basic-triangle))
  (scale-by (* 0.5 (width *context*)) (* 0.5 (height *context*)) 1)
  (translate-by +1 -1 0)
  (rotate +vx+ (* 1.0 (current-time)))
  (rotate +vy+ (* 1.2 (current-time))))

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
    (nmortho (projection-matrix) -10 +1270 -700 20 0 1)
    (enter (make-instance 'basic-triangle) scene)
    (enter (make-instance 'bouncer) scene)
    (enter (make-instance 'trial::repl :foreground (vec 1 1 1 1)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
