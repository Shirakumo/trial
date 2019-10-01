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
           (data (list NIL "" 50 NIL 100 :male))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui))
           (save (alloy:represent "Save" 'alloy:button :renderer ui))
           (autosave (alloy:represent (first data) 'alloy:switch :renderer ui))
           (title (alloy:represent (second data) 'alloy:input-line :renderer ui))
           (progress (alloy:represent (third data) 'alloy:progress :renderer ui))
           (option (alloy:place-data (fourth data)))
           (option-a (alloy:represent-with 'alloy:radio option :active-value :on :renderer ui))
           (option-b (alloy:represent-with 'alloy:radio option :active-value :off :renderer ui))
           (volume (alloy:represent (fifth data) 'alloy:slider :renderer ui))
           (voices (alloy:represent (sixth data) 'alloy::combo-set :renderer ui :value-set '(:male :female :none))))
      (let* ((focus (make-instance 'alloy:focus-list :focus-parent focus))
             (layout (make-instance 'alloy:vertical-linear-layout :layout-parent layout
                                                                  :renderer ui
                                                                  :cell-margins (alloy:margins 3)
                                                                  :min-size (alloy:size 200 30))))
        (alloy:enter save layout)
        (alloy:enter progress layout)
        (let* ((inner (make-instance 'alloy:grid-layout :layout-parent layout
                                                        :cell-margins (alloy:margins 5)
                                                        :col-sizes (list 200 T)
                                                        :row-sizes (list 40 40 40 40 40 40))))
          (alloy:enter inner layout)
          (alloy:enter "Autosave" inner :col 0 :row 0)
          (alloy:enter autosave inner :col 1 :row 0)
          (alloy:enter "Title" inner :col 0 :row 1)
          (alloy:enter title inner :col 1 :row 1)
          (alloy:enter "Feelings On" inner :col 0 :row 2)
          (alloy:enter option-a inner :col 1 :row 2)
          (alloy:enter "Feelings Off" inner :col 0 :row 3)
          (alloy:enter option-b inner :col 1 :row 3)
          (alloy:enter "Volume" inner :col 0 :row 4)
          (alloy:enter volume inner :col 1 :row 4)
          (alloy:enter "Voice" inner :col 0 :row 5)
          (alloy:enter voices inner :col 1 :row 5)))
      (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
        (alloy:enter autosave focus)
        (alloy:enter title focus)
        (alloy:enter option-a focus)
        (alloy:enter option-b focus)
        (alloy:enter volume focus)
        (alloy:enter save focus)))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
