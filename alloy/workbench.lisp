#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass workbench (trial:main) ()
  (:default-initargs :clear-color (vec 0.3 0.45 0.4)))

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (let* ((ui (trial:enter (make-instance 'ui) scene))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui))
           (button (make-instance 'alloy:button :text "Foo"))
           (button2 (make-instance 'alloy:button :text "Bar")))
      (let ((layout (make-instance 'alloy:vertical-linear-layout :parent layout
                                                                 :min-size (alloy:size 200 30))))
        (alloy:enter button layout)
        (alloy:enter button2 layout))
      (let ((focus (make-instance 'alloy:focus-list :parent focus)))
        (alloy:enter button focus)
        (alloy:enter button2 focus)))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
