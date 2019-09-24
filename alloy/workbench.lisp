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
           (data (list NIL ""))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui))
           (save (alloy:represent "Save" 'alloy:button :renderer ui
                                                       :style `((:background :fill-color ,(simple:color 1 0 0)))))
           (autosave (alloy:represent (first data) 'alloy:switch :renderer ui))
           (title (alloy:represent (second data) 'alloy:input-line :renderer ui)))
      (let* ((focus (make-instance 'alloy:focus-list :focus-parent focus))
             (layout (make-instance 'alloy:vertical-linear-layout :layout-parent layout
                                                                  :renderer ui
                                                                  :min-size (alloy:size 200 30))))
        (alloy:enter save layout)
        (let* ((inner (make-instance 'alloy:grid-layout :layout-parent layout
                                                        :cell-margins (alloy:margins 2)
                                                        :col-sizes #(100 T)
                                                        :row-sizes #(30 30))))
          (alloy:enter inner layout)
          (alloy:enter "Autosave" inner :col 0 :row 0)
          (alloy:enter autosave inner :col 1 :row 0)
          (alloy:enter "Title" inner :col 0 :row 1)
          (alloy:enter title inner :col 1 :row 1)
          (alloy:enter autosave focus)
          (alloy:enter title focus)
          (alloy:enter save focus))))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
