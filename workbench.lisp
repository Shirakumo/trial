(in-package #:trial)

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'target-camera :location (vec 70 -100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
