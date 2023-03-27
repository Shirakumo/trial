(in-package #:workbench)

(define-asset (workbench simple) org.shirakumo.fraf.trial.gltf:asset
    ;; #p "~/Projects/var/gltf-samples/2.0/Box/glTF/Box.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/BoxTextured/glTF/BoxTextured.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/SimpleSkin/glTF/SimpleSkin.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/RiggedSimple/glTF/RiggedSimple.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/RiggedFigure/glTF/RiggedFigure.gltf"
    #p "~/Projects/cl/trial-assets/data/Woman.gltf"
  )

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
    (disable :cull-face)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'player :asset (asset 'workbench 'simple)) scene)
    (enter (make-instance 'target-camera :target (vec 0 2 0) :location (vec 0 3 5)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
