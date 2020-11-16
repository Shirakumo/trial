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

(defmethod initialize-instance :before ((workbench workbench) &key)
  (harmony:maybe-start-simple-server))

(defmethod finalize ((workbench workbench))
  (when harmony:*server*
    (harmony:end harmony:*server*)))

(progn
  (defmethod setup-scene ((workbench workbench) scene))
  (maybe-reload-scene))
