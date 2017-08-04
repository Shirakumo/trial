#|
This file is a part of trial
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget list-inspector (QDialog inspector)
  ((object)))

(define-initializer (list-inspector setup)
  (setf (q+:window-title list-inspector) "List Inspector")
  (q+:resize list-inspector 300 600)
  (refresh-instances list-inspector))

(define-subwidget (list-inspector entries)
    (make-instance 'list-listing :inspector list-inspector))

(define-subwidget (list-inspector scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget scroller) entries)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off))
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on)))

(define-subwidget (list-inspector add)
    (q+:make-qpushbutton)
  (setf (q+:icon add) (icon :add))
  (setf (q+:tool-tip add) "Add a list element."))

(define-subwidget (list-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (icon :refresh))
  (setf (q+:tool-tip refresh) "Refresh the list of entries."))

(define-subwidget (list-inspector layout)
    (q+:make-qgridlayout list-inspector)
  (q+:add-widget layout scroller 0 0 1 2)
  (q+:add-widget layout refresh 1 0 1 1)
  (q+:add-widget layout add 1 1 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (list-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (qui:clear-layout entries T)
  (refresh-background list-inspector))

(defmethod refresh-background ((list-inspector list-inspector))
  (with-slots-bound (list-inspector list-inspector)
    (loop for cons on object
          do (add-item cons list-inspector)
             (sleep 0.01))))

(define-slot (list-inspector add-item) ((item qobject))
  (declare (connected list-inspector (add-item qobject)))
  (when (typep item 'signal-carrier)
    (qui:add-item (object item) entries)))

(define-slot (list-inspector add) ()
  (declare (connected add (clicked)))
  (multiple-value-bind (value got) (safe-input-value list-inspector)
    (when got
      (setf (cdr object) (cons value (cdr object)))
      (rotatef (car object) (cadr object))
      (refresh-instances list-inspector))))

(define-widget list-listing (QWidget qui:listing)
  ((inspector :initarg :inspector :accessor inspector)))

(defmethod qui:coerce-item (index (listing list-listing))
  (make-instance 'list-listing-widget :item index :container listing))

(define-widget list-listing-widget (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-subwidget (list-listing-widget value-button)
    (let ((cons (qui:widget-item list-listing-widget)))
      (q+:make-qpushbutton (safe-prin1 (car cons)))))

(define-subwidget (list-listing-widget set-value)
    (make-instance 'inline-button :icon :set :tooltip "Set the element to a new value."))

(define-subwidget (list-listing-widget remove-entry)
    (make-instance 'inline-button :icon :remove :tooltip "Remove the element."))

(define-subwidget (list-listing-widget layout)
    (q+:make-qhboxlayout list-listing-widget)
  (q+:add-widget layout value-button)
  (q+:add-widget layout set-value)
  (q+:add-widget layout remove-entry)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0))

(define-slot (list-listing-widget inspect-value) ()
  (declare (connected value-button (clicked)))
  (let ((cons (qui:widget-item list-listing-widget)))
    (inspect (car cons))))

(define-slot (list-listing-widget set-value) ()
  (declare (connected set-value (clicked)))
  (let ((cons (qui:widget-item list-listing-widget)))
    (multiple-value-bind (value got) (safe-input-value list-listing-widget)
      (when got
        (setf (car cons) value)
        (setf (q+:text value-button) (safe-prin1 value))))))

(define-slot (list-listing-widget remove-entry) ()
  (declare (connected remove-entry (clicked)))
  (let ((cons (qui:widget-item list-listing-widget)))
    (cond ((cdr cons)
           (setf (car cons) (cadr cons))
           (setf (cdr cons) (cddr cons))
           (refresh-instances (inspector (qui:container list-listing-widget))))
          (T
           (setf (car cons) NIL)
           (setf (q+:text value-button) (safe-prin1 NIL))))))
