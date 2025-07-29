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

(defmacro ! (&body body)
  `(when (and +main+ (scene +main+))
     (with-eval-in-render-loop ()
       ,@body)))

(defun n (object &optional (container T))
  (node object container))

(define-pool workbench)

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
