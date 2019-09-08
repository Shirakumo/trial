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
    (let* ((ui (trial:enter (make-instance 'dui) scene))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui))
           (save (make-instance 'alloy:button :text "Save"))
           (autosave (make-instance 'alloy:switch))
           (title (make-instance 'alloy:input-line)))
      (let ((layout (make-instance 'alloy:vertical-linear-layout :parent layout
                                                                 :min-size (alloy:size 200 30))))
        (alloy:enter save layout)
        (let ((inner (make-instance 'alloy:grid-layout :parent layout
                                                       :cell-margins (alloy:margins :l 2 :u 2 :r 2 :b 2)
                                                       :col-sizes #(100 200)
                                                       :row-sizes #(30 30))))
          (alloy:enter inner layout)
          (alloy:enter "Autosave" inner :col 0 :row 0)
          (alloy:enter autosave inner :col 1 :row 0)
          (alloy:enter "Title" inner :col 0 :row 1)
          (alloy:enter title inner :col 1 :row 1)))
      (let ((focus (make-instance 'alloy:focus-list :parent focus)))
        (alloy:enter autosave focus)
        (alloy:enter title focus)
        (alloy:enter save focus))
      (alloy:register ui ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
