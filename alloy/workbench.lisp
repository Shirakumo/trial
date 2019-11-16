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
      (let* ((focus (make-instance 'alloy:focus-list :focus-parent focus))
             (layout (make-instance 'alloy:fixed-layout :layout-parent layout))
             (window (make-instance 'alloy:window)))
        (alloy:enter window layout :x 50 :y 100 :w 300 :h 500)
        (alloy:enter window focus)
        (let* ((name "")
               (content (make-instance 'alloy:vertical-linear-layout :align :end :cell-margins (alloy:margins 5)))
               (focus* (make-instance 'alloy:focus-list))
               (input (alloy:represent name 'alloy:input-line :focus-parent focus* :layout-parent content))
               (button (alloy:represent "Greet" 'alloy:button  :focus-parent focus* :layout-parent content)))
          (alloy:enter content window)
          (alloy:enter focus* window)
          (alloy:on alloy:activate (button)
            (let ((window (make-instance 'alloy:window :title "Hello!"))
                  (label (alloy:represent-with 'alloy:label (format NIL "Hey there, ~a." name))))
              (alloy:enter label window)
              (alloy:enter window layout :x (alloy:vw 0.5) :y (alloy:vh 0.5) :w 200 :h 60)
              (alloy:enter window focus)
              (alloy:register window ui)))))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
