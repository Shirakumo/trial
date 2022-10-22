(in-package #:org.shirakumo.fraf.trial.animation)

(defclass workbench (trial:main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)
                     :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(trial:define-pool workbench)

(trial:define-asset (workbench simple) gltf-asset
    ;; #p "~/Projects/var/gltf-samples/2.0/Box/glTF/Box.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/BoxTextured/glTF/BoxTextured.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/SimpleSkin/glTF/SimpleSkin.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/RiggedSimple/glTF/RiggedSimple.gltf"
    ;; #p "~/Projects/var/gltf-samples/2.0/RiggedFigure/glTF/RiggedFigure.gltf"
    #p "~/Projects/cl/trial-assets/data/Woman.gltf"
    )

(trial:define-asset (workbench grid) trial:mesh
    (trial:make-line-grid 10 10 10))

(trial:define-shader-entity player (entity)
  ((trial:name :initform 'player)))

(trial:define-handler (player trial:tick :after) (trial:dt)
  (let ((vel (vec 0 0 0)))
    (when (trial:retained :w) (incf (vz vel) 10.0))
    (when (trial:retained :s) (decf (vz vel) 10.0))
    (if (v/= vel 0)
        (fade-to "Running" player)
        (fade-to "Idle" player))
    (nv+ (tlocation (trial:tf player)) (nv* vel trial:dt))))

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (trial:disable :cull-face)
    (trial:enter (make-instance 'trial::fps-counter) scene)
    (trial:enter (make-instance 'trial:vertex-entity :vertex-array (trial:// 'workbench 'grid)) scene)
    (trial:enter (make-instance 'player :asset (trial:asset 'workbench 'simple)) scene)
    (trial:enter (make-instance 'trial:target-camera :target (vec 0 2 0) :location (vec 0 3 5)) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))
  (trial:maybe-reload-scene))
