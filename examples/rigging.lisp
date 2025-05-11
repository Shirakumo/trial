(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity rigging-entity (basic-animated-entity listener)
  ())

(define-handler ((entity rigging-entity) tick :after) (dt)
  (let ((vel (vec 0 0 0)))
    (when (retained :w) (incf (vz vel) 10.0))
    (when (retained :s) (decf (vz vel) 10.0))
    (if (v/= vel 0)
        (fade-to :Running entity)
        (fade-to :Idle entity))
    (nv+ (tlocation (local-transform entity)) (nv* vel dt))
    (handle tick (animation-controller entity))))

(define-example rigging
  :title "Animated Model"
  :description "An example showing a rigged 3D model."
  (enter (make-instance 'directional-light) scene)
  (enter (make-instance 'ambient-light :color (vec3 0.25)) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'rigging-entity :name :rig :asset (assets:asset :woman) :scaling (vec3 0.01)) scene)
  (enter (make-instance 'target-camera :target (vec 0 2 0) :location (vec 0 3 5)) scene)
  (enter (make-instance 'phong-render-pass) scene))

(defmethod setup-ui ((scene rigging-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(120 140 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (alloy:enter "Palette Type" layout :row 0 :col 0)
    (alloy:represent (trial::palette-type (animation-controller (node :rig SCENE)))
                     'alloy:combo-set :value-set '(mat4 quat2) :layout-parent layout :focus-parent focus)
    (alloy:finish-structure panel layout focus)))
