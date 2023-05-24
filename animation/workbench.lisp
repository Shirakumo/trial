(in-package #:workbench)

(define-shader-entity player (animated-entity)
  ((name :initform 'player)))

(define-handler (player tick :after) (dt)
  (let ((vel (vec 0 0 0)))
    (when (retained :w) (incf (vz vel) 10.0))
    (when (retained :s) (decf (vz vel) 10.0))
    (if (v/= vel 0)
        (fade-to "Running" player)
        (fade-to "Idle" player))
    (nv+ (tlocation (tf player)) (nv* vel dt))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (disable-feature :cull-face)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'animated-entity :asset (assets:asset :woman) :location (vec -2 0 0)) scene)
    (enter (make-instance 'trial::dquat-animated-entity :asset (assets:asset :woman) :location (vec +2 0 0)) scene)
    (enter (make-instance 'target-camera :target (vec 0 2 0) :location (vec 0 3 5)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
