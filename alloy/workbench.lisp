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

(shadow "PHASE")
(defclass sine-generator (alloy:data)
  ((frequency :initform 10 :accessor frequency)
   (phase :initform 0 :accessor phase)
   (amplitude :initform 50 :accessor amplitude)))

(defmethod (setf frequency) :after (value (generator sine-generator))
  (alloy:notify-observers '(setf alloy:value) generator (value generator) generator))

(defmethod (setf phase) :after (value (generator sine-generator))
  (alloy:notify-observers '(setf alloy:value) generator (value generator) generator))

(defmethod (setf amplitude) :after (value (generator sine-generator))
  (alloy:notify-observers '(setf alloy:value) generator (value generator) generator))

(defmethod alloy:value ((generator sine-generator))
  (let ((range (* 2 PI (frequency generator))))
    (coerce (loop for i from 0 below 1000
                  for phi = (+ (phase generator) (* range (/ i 1000)))
                  collect (* (sin phi) (amplitude generator)))
            'vector)))

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
      (let* ((generator (make-instance 'sine-generator))
             (freq (alloy:represent (frequency generator) 'alloy:ranged-slider :range '(.1 . 100) :step 0.1))
             (phas (alloy:represent (phase generator) 'alloy:ranged-slider :range (cons 0 PI)))
             (amps (alloy:represent (amplitude generator) 'alloy:ranged-slider :range '(0 . 100)))
             (plot (alloy:represent-with 'alloy:plot generator :y-range '(-100 . 100))))
        (let ((l1 (make-instance 'alloy:vertical-linear-layout :layout-parent layout :cell-margins (alloy:margins 5))))
          (let ((l2 (make-instance 'alloy:grid-layout :layout-parent l1
                                                      :cell-margins (alloy:margins 5)
                                                      :col-sizes (list (alloy:un 100) T)
                                                      :row-sizes (list (alloy:un 30) (alloy:un 30) (alloy:un 30)))))
            (alloy:enter "Frequency" l2 :row 2 :col 0)
            (alloy:enter freq l2 :row 2 :col 1)
            (alloy:enter "Amplitude" l2 :row 1 :col 0)
            (alloy:enter amps l2 :row 1 :col 1)
            (alloy:enter "Phase" l2 :row 0 :col 0)
            (alloy:enter phas l2 :row 0 :col 1))
          (alloy:enter plot l1))
        (let ((focus (make-instance 'alloy:focus-list :focus-parent focus)))
          (alloy:enter freq focus)
          (alloy:enter amps focus)
          (alloy:enter phas focus)))
      (alloy:register layout ui))
    (trial:enter (make-instance 'trial:2d-camera) scene)
    (trial:enter (make-instance 'trial:render-pass) scene))

  (trial:maybe-reload-scene))
