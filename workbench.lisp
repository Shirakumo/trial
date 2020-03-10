(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-shader-subject grid (vertex-entity)
  ((vertex-array :initform (asset 'workbench 'grid))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (disable :cull-face)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'editor-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
