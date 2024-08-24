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

(define-shader-entity basic-triangle (vertex-entity vertex-colored-entity)
  ((vertex-array :initform (// 'workbench 'triangle))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (!meye (view-matrix))
    (nmortho (projection-matrix) -10 +1270 -700 20 0 1)
    ;;(enter (make-instance 'basic-triangle) scene)
    (enter (make-instance 'trial::repl :foreground (vec 1 1 1 1))
           scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
