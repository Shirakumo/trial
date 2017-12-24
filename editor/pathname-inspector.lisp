#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget pathname-inspector (QDialog inspector)
  ((object)))

(define-initializer (pathname-inspector setup)
  (setf (q+:window-title pathname-inspector) (format NIL "Pathname Inspector for ~a" object))
  (q+:resize pathname-inspector 400 200))

(define-subwidget (pathname-inspector layout)
    (q+:make-qformlayout pathname-inspector)
  (flet ((add-row (name value)
           (let ((label (q+:make-qlabel (princ-to-string value))))
             (q+:add-row layout name label)
             (setf (q+:alignment label) (q+:qt.align-right)))))
    (add-row "Namestring" (uiop:native-namestring object))
    (add-row "Name" (pathname-name object))
    (add-row "Type" (pathname-type object))
    (add-row "Directory" (pathname-directory object))
    (add-row "Device" (pathname-device object))
    (add-row "Host" (pathname-host object))
    (add-row "Version" (pathname-version object))))
