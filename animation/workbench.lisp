(in-package #:org.shirakumo.fraf.trial.animation)
(ql:quickload :trial-glfw)

(defclass workbench (trial:main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)
                     :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(trial:define-pool workbench)

(trial:define-asset (workbench simple) gltf-asset
    #p "~/Projects/var/gltf-samples/2.0/SimpleSkin/glTF/SimpleSkin.gltf")

(trial:define-asset (workbench grid) trial:mesh
    (trial:make-line-grid 10 100 100))

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (trial:enter (make-instance 'trial::fps-counter) scene)
    (trial:enter (make-instance 'trial:vertex-entity :vertex-array (trial:// 'workbench 'grid)) scene)
    (trial:enter (make-instance 'entity :asset (trial:asset 'workbench 'simple)) scene)
    (trial:enter (make-instance 'trial:editor-camera :location (vec 0 100 100)) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))
  (trial:maybe-reload-scene))
