(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity rigging-entity (animated-entity vertex-entity)
  ())

(define-handler ((entity rigging-entity) tick :after) (dt)
  (let ((vel (vec 0 0 0)))
    (when (retained :w) (incf (vz vel) 10.0))
    (when (retained :s) (decf (vz vel) 10.0))
    (if (v/= vel 0)
        (fade-to "Running" entity)
        (fade-to "Idle" entity))
    (nv+ (tlocation (tf entity)) (nv* vel dt))))

(define-example rigging
  (disable-feature :cull-face)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'rigging-entity :asset (assets:asset :woman)) scene)
  (enter (make-instance 'target-camera :target (vec 0 2 0) :location (vec 0 3 5)) scene)
  (enter (make-instance 'render-pass) scene))
