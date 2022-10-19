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
    #p "~/Projects/cl/trial-assets/data/Woman.gltf")

(trial:define-asset (workbench grid) trial:mesh
    (trial:make-line-grid 10 10 10))

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (trial:disable :cull-face)
    (trial:enter (make-instance 'trial::fps-counter) scene)
    (trial:enter (make-instance 'trial:vertex-entity :vertex-array (trial:// 'workbench 'grid)) scene)
    ;(trial:enter (make-instance 'entity :name 'entity :asset (trial:asset 'workbench 'simple)) scene)
    (trial:enter (make-instance 'lines :name 'entity :asset (trial:asset 'workbench 'simple)) scene)
    (trial:enter (make-instance 'trial:target-camera :target (vec 0 1 0) :location (vec 0 2 -3)) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))
  (trial:maybe-reload-scene))
