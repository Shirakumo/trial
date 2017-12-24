#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget cons-inspector (QDialog inspector)
  ((object)))

(define-initializer (cons-inspector setup)
  (setf (q+:window-title cons-inspector) "Cons Inspector")
  (q+:resize cons-inspector 300 100)
  (refresh-instances cons-inspector))

(define-subwidget (cons-inspector car-button)
    (q+:make-qpushbutton (safe-prin1 (car object))))

(define-subwidget (cons-inspector set-car)
    (make-instance 'inline-button :icon :set :tooltip "Set the car to a new value."))

(define-subwidget (cons-inspector cdr-button)
    (q+:make-qpushbutton (safe-prin1 (cdr object))))

(define-subwidget (cons-inspector set-cdr)
    (make-instance 'inline-button :icon :set :tooltip "Set the cdr to a new value."))

(define-subwidget (cons-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (icon :refresh))
  (setf (q+:tool-tip refresh) "Refresh the cons structure."))

(define-subwidget (cons-inspector layout)
    (q+:make-qgridlayout cons-inspector)
  (q+:add-widget layout car-button 0 0 1 1)
  (q+:add-widget layout set-car 0 1 1 1)
  (q+:add-widget layout cdr-button 0 2 1 1)
  (q+:add-widget layout set-cdr 0 3 1 1)
  (q+:add-widget layout refresh 1 0 1 4)
  (setf (q+:spacing layout) 0))

(define-slot (cons-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (setf (q+:text car-button) (safe-prin1 (car object)))
  (setf (q+:text cdr-button) (safe-prin1 (cdr object))))

(define-slot (cons-inspector inspect-car) ()
  (declare (connected car-button (clicked)))
  (inspect (car object)))

(define-slot (cons-inspector set-car) ()
  (declare (connected set-car (clicked)))
  (multiple-value-bind (value got) (safe-input-value cons-inspector)
    (when got
      (setf (car object) value)
      (refresh-instances cons-inspector))))

(define-slot (cons-inspector inspect-cdr) ()
  (declare (connected cdr-button (clicked)))
  (inspect (cdr object)))

(define-slot (cons-inspector set-cdr) ()
  (declare (connected set-cdr (clicked)))
  (multiple-value-bind (value got) (safe-input-value cons-inspector)
    (when got
      (setf (cdr object) value)
      (refresh-instances cons-inspector))))
