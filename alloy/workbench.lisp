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
      ;; (let* ((layout (make-instance 'alloy:border-layout :layout-parent layout
      ;;                                                    :renderer ui))
      ;;        (c (make-instance 'alloy:clip-view :layout-parent layout))
      ;;        (x (alloy:represent-with 'alloy:x-scrollbar c :renderer ui))
      ;;        (y (alloy:represent-with 'alloy:y-scrollbar c :renderer ui))
      ;;        (list (make-instance 'alloy:horizontal-linear-layout :layout-parent c :min-size (alloy:px-size 200 30))))
      ;;   (alloy:enter y layout :place :east :size (alloy:un 20))
      ;;   (alloy:enter x layout :place :south :size (alloy:un 20))
      ;;   (dotimes (i 20)
      ;;     (alloy:enter (princ-to-string i) list))
      ;;   (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
      ;;     (alloy:enter y focus)
      ;;     (alloy:enter x focus)))
      (let* ((data (list "" 1 T))
             (plot #(1 10 2 100 90 50 10 0))
             (c1 (alloy:represent (first data) 'alloy:input-line))
             (c2 (alloy:represent (second data) 'alloy:ranged-slider))
             (c3 (alloy:represent (third data) 'alloy:switch))
             (c4 (alloy:represent "Go" 'alloy:button))
             (c5 (alloy:represent plot 'alloy:plot)))
        (let ((l1 (make-instance 'alloy:vertical-linear-layout :layout-parent layout :cell-margins (alloy:margins 5))))
          (alloy:enter c4 l1)
          (let ((l2 (make-instance 'alloy:grid-layout :layout-parent l1
                                                      :cell-margins (alloy:margins 5)
                                                      :col-sizes (list (alloy:un 100) T)
                                                      :row-sizes (list (alloy:un 30) (alloy:un 30) (alloy:un 30)))))
            (alloy:enter "Name" l2 :row 2 :col 0)
            (alloy:enter c1 l2 :row 2 :col 1)
            (alloy:enter "Age" l2 :row 1 :col 0)
            (alloy:enter c2 l2 :row 1 :col 1)
            (alloy:enter "Skeleton" l2 :row 0 :col 0)
            (alloy:enter c3 l2 :row 0 :col 1))
          (alloy:enter c5 l1))
        (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
          (alloy:enter c1 focus)
          (alloy:enter c2 focus)
          (alloy:enter c3 focus)
          (alloy:enter c4 focus)))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
