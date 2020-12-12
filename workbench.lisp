(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:harmony #:org.shirakumo.fraf.harmony.user)
   (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(defun launch ()
  (trial:launch 'workbench))

(define-pool workbench)

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (disable :cull-face)
    (disable :depth-test)
    (enter (make-instance 'trial::fps-counter) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
