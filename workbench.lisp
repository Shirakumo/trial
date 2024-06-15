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
  (:default-initargs :context '(:vsync T)))

(defmethod update ((main workbench) tt dt fc)
  (if (paused-p main)
      (handle (make-event 'tick :tt tt :dt dt :fc fc) (camera (scene main)))
      (issue (scene main) 'tick :tt tt :dt dt :fc fc))
  (process (scene main)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench sample) model-file
  #p "~/Downloads/MorphPrimitivesTest.glb")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
    (enter (make-instance 'standard-animated-renderable :asset (asset 'workbench 'sample)) scene)
    (enter (make-instance 'phong-render-pass) scene))
  (maybe-reload-scene))
