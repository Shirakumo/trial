#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass workbench (trial:main) ()
  (:default-initargs :clear-color (vec 0.1 0.1 0.1)))

(defclass dui (ui org.shirakumo.alloy.renderers.simple.presentations::default-look-and-feel)
  ())

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (let* ((ui (trial:enter (make-instance 'dui :target-resolution (alloy:px-size 800 600)) scene))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui)))
      (let* ((tabs (make-instance 'alloy:tab-view
                                  :tabs (loop for i from 0 to 10
                                              for button = (alloy:represent-with 'alloy:button (princ-to-string i))
                                              collect (make-instance 'alloy:tab :name (princ-to-string i) :focus-element button :layout-element button))))
             (window (make-instance 'alloy:window :layout tabs :focus tabs))
             (focus (make-instance 'alloy:focus-list :focus-parent focus))
             (layout (make-instance 'alloy:fixed-layout :layout-parent layout)))
        (alloy:enter window layout :x 10 :y 10 :w 300 :h 500)
        (alloy:enter window focus))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
