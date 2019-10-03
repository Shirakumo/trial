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
      (let* ((layout (make-instance 'alloy:border-layout :layout-parent layout
                                                         :renderer ui))
             (c (make-instance 'alloy:clip-view :layout-parent layout))
             (x (alloy:represent-with 'alloy:x-scrollbar c :renderer ui))
             (y (alloy:represent-with 'alloy:y-scrollbar c :renderer ui))
             (list (make-instance 'alloy:horizontal-linear-layout :layout-parent c :min-size (alloy:px-size 200 30))))
        (alloy:enter y layout :place :east :size (alloy:un 20))
        (alloy:enter x layout :place :south :size (alloy:un 20))
        (dotimes (i 20)
          (alloy:enter (princ-to-string i) list))
        (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
          (alloy:enter y focus)
          (alloy:enter x focus))))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
