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
      (let* ((path "/home/linus/")
             (layout (make-instance 'alloy:border-layout :layout-parent layout))
             (c (make-instance 'alloy:clip-view :layout-parent layout))
             (x (alloy:represent-with 'alloy:x-scrollbar c))
             (p (alloy:represent path 'alloy:input-line))
             (g (alloy:represent "Go" 'alloy:button))
             (list (make-instance 'alloy:horizontal-linear-layout :layout-parent c :min-size (alloy:px-size 200 30)))
             (top (make-instance 'alloy:grid-layout :layout-parent NIL :col-sizes '(T 50) :row-sizes '(30))))
        (alloy:enter x layout :place :south :size (alloy:un 20))
        (alloy:enter top layout :place :north :size (alloy:un 30))
        (alloy:enter p top :row 0 :col 0)
        (alloy:enter g top :row 0 :col 1)
        (alloy:on alloy:activate (g)
          (alloy:clear list)
          (loop for image in (directory (make-pathname :name :wild :type "png" :defaults path))
                do (alloy:register (alloy:enter (simple:request-image ui image) list) ui)))
        (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
          (alloy:enter p focus)
          (alloy:enter g focus)
          (alloy:enter x focus)))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
