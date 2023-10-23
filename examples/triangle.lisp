(in-package #:org.shirakumo.fraf.trial.examples)

(define-asset (examples triangle) mesh
    (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'colored-vertex))
      (vertex :position (vec -0.5 -0.5 0.0) :color (vec 1 0 0 1))
      (vertex :position (vec +0.5 -0.5 0.0) :color (vec 0 1 0 1))
      (vertex :position (vec +0.0 +0.5 0.0) :color (vec 0 0 1 1))))

(define-shader-entity basic-triangle (vertex-entity vertex-colored-entity)
  ((vertex-array :initform (// 'examples 'triangle))))

(define-example triangle
  :title "Basic Triangle"
  (enter (make-instance 'basic-triangle) scene)
  (enter (make-instance 'render-pass) scene))
