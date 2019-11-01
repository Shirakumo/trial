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

(defclass data (alloy:observable-object)
  ((frequency :initform 1 :accessor frequency)
   (wavephase :initform 0 :accessor wavephase)
   (amplitude :initform 100 :accessor amplitude)))

(progn
  (defmethod trial:setup-scene ((workbench workbench) scene)
    (let* ((ui (trial:enter (make-instance 'dui :target-resolution (alloy:px-size 800 600)) scene))
           (focus (alloy:focus-tree ui))
           (layout (alloy:layout-tree ui)))
      (let* ((data (make-instance 'data))
             (freq (alloy:represent (frequency data) 'alloy:ranged-slider :range '(.1 . 100) :step 0.1))
             (phas (alloy:represent (wavephase data) 'alloy:ranged-slider :range (cons 0 PI)))
             (amps (alloy:represent (amplitude data) 'alloy:ranged-slider :range '(0 . 100)))
             (plot (alloy:represent (lambda ((freq (frequency data))
                                             (amps (amplitude data))
                                             (phas (wavephase data)))
                                      (coerce (loop with range = (* 2 PI freq)
                                                    for i from 0 below 1000
                                                    for phi = (+ phas (* range (/ i 1000)))
                                                    collect (* (sin phi) amps))
                                              'vector))
                    'alloy:plot :y-range '(-100 . 100) :style `((:curve :line-width ,(alloy:un 5))))))
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
