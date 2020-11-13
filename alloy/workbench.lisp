#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass workbench (trial:main) ()
  (:default-initargs :clear-color (3d-vectors:vec 0.1 0.1 0.1)))

(defclass dui (ui
               alloy:smooth-scaling-ui
               org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ())

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (let* ((ui (trial:enter (make-instance 'dui :target-resolution (alloy:px-size 800 600)) scene))
           (focus (make-instance 'alloy:focus-list :focus-parent (alloy:focus-tree ui)))
           (layout (make-instance 'alloy:vertical-linear-layout :layout-parent (alloy:layout-tree ui))))
      (let* ((data (3d-vectors:vec2 0 1))
             (vec (alloy:represent data 'vec2 :focus-parent focus :layout-parent layout)))
        (alloy:on alloy:value (value vec)
          (print value)))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
