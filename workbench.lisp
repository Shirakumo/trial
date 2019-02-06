(in-package #:trial)

(defclass workbench (main) ())

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((workbench workbench) scene))
  (maybe-reload-scene))


