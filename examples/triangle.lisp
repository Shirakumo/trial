(in-package #:org.shirakumo.fraf.trial.examples)

(define-asset (examples triangle) mesh
    (with-mesh-construction (v :attributes (location color))
      (v -0.5 -0.5 0.0 1 0 0 1)
      (v +0.5 -0.5 0.0 0 1 0 1)
      (v +0.0 +0.5 0.0 0 0 1 1)
      (finalize-data)))

(define-shader-entity basic-triangle (vertex-entity vertex-colored-entity)
  ((vertex-array :initform (// 'examples 'triangle))))

(define-example triangle
  :title "Basic Triangle"
  :description "The famed rainbow triangle demo."
  (!meye (view-matrix))
  (!meye (projection-matrix))
  (enter (make-instance 'basic-triangle) scene)
  (enter (make-instance 'render-pass) scene))
