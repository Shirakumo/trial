#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(define-widget cons-inspector (QDialog)
  ((object :initarg :object :accessor object))
  (:default-initargs :object (error "OBJECT is required.")))

(define-initializer (cons-inspector setup)
  (setf (q+:window-title cons-inspector) "Cons Inspector")
  (q+:resize cons-inspector 300 100)
  (refresh-instances cons-inspector))

(define-subwidget (cons-inspector car-button)
    (q+:make-qpushbutton (safe-princ (car object))))

(define-subwidget (cons-inspector set-car)
    (q+:make-qpushbutton)
  (setf (q+:icon set-car) (q+:standard-icon (q+:style set-car)
                                            (q+:qstyle.sp_arrow-left)))
  (setf (q+:tool-tip set-car) "Set the car to a new value.")
  (setf (q+:fixed-width set-car) 40))

(define-subwidget (cons-inspector cdr-button)
    (q+:make-qpushbutton (safe-princ (cdr object))))

(define-subwidget (cons-inspector set-cdr)
    (q+:make-qpushbutton)
  (setf (q+:icon set-cdr) (q+:standard-icon (q+:style set-cdr)
                                            (q+:qstyle.sp_arrow-left)))
  (setf (q+:tool-tip set-cdr) "Set the cdr to a new value.")
  (setf (q+:fixed-width set-cdr) 40))

(define-subwidget (cons-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (q+:standard-icon (q+:style refresh)
                                            (q+:qstyle.sp_browser-reload)))
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
  (setf (q+:text car-button) (safe-princ (car object)))
  (setf (q+:text cdr-button) (safe-princ (cdr object))))

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
