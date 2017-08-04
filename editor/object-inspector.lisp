#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget object-inspector (QDialog inspector)
  ((object)))

(define-initializer (object-inspector setup)
  (setf (q+:window-title object-inspector) (format NIL "Inspecting ~a" (safe-princ object)))
  (q+:resize object-inspector 500 600)
  (refresh-instances object-inspector))

(define-subwidget (object-inspector instance-label)
    (q+:make-qlabel (safe-princ object))
  (setf (q+:alignment instance-label) (q+:qt.align-center))
  (setf (q+:style-sheet instance-label) "margin: 2px; font-size: 16pt;"))

(define-subwidget (object-inspector class-button)
    (q+:make-qpushbutton (prin1-to-string (class-name (class-of object)))))

(define-subwidget (object-inspector docstring)
    (q+:make-qlabel (or (documentation (class-of object) T) "<No documentation>"))
  (setf (q+:word-wrap docstring) T)
  (setf (q+:style-sheet docstring) "margin: 10px;"))

(define-subwidget (object-inspector slots)
    (make-instance 'slot-listing :object object))

(define-subwidget (object-inspector scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget scroller) slots)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off))
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on)))

(define-subwidget (object-inspector reinitialize)
    (q+:make-qpushbutton)
  (setf (q+:icon reinitialize) (icon :reinitialize))
  (setf (q+:tool-tip reinitialize) "Reinitialize the instance."))

(define-subwidget (object-inspector finalize)
    (q+:make-qpushbutton)
  (setf (q+:icon finalize) (icon :finalize))
  (setf (q+:tool-tip finalize) "Finalize the instance."))

(define-subwidget (object-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (icon :refresh))
  (setf (q+:tool-tip refresh) "Refresh the list of slots."))

(define-subwidget (object-inspector layout)
    (q+:make-qgridlayout object-inspector)
  (q+:add-widget layout class-button 0 0 1 3)
  (q+:add-widget layout instance-label 1 0 1 3)
  (q+:add-widget layout docstring 2 0 1 3)
  (q+:add-widget layout scroller 3 0 1 3)
  (q+:add-widget layout refresh 4 0 1 1)
  (q+:add-widget layout reinitialize 4 1 1 1)
  (q+:add-widget layout finalize 4 2 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (object-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (qui:clear-layout slots T)
  (dolist (slot (c2mop:class-slots (class-of object)))
    (qui:add-item (c2mop:slot-definition-name slot) slots)))

(define-slot (object-inspector inspect-class) ()
  (declare (connected class-button (clicked)))
  (inspect (class-of object)))

(define-slot (object-inspector reinitialize) ()
  (declare (connected reinitialize (clicked)))
  (reinitialize-instance object))

(define-slot (object-inspector finalize) ()
  (declare (connected finalize (clicked)))
  (trial:finalize object))

(define-widget slot-listing (QWidget qui:listing)
  ((object :initarg :object :accessor object)))

(defmethod qui:coerce-item ((item symbol) (listing slot-listing))
  (make-instance 'slot-listing-widget :item item :container listing))

(define-widget slot-listing-widget (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-subwidget (slot-listing-widget name)
    (q+:make-qlabel (format NIL "~(~s~)" (qui:widget-item slot-listing-widget)))
  (setf (q+:fixed-width name) 200))

(define-subwidget (slot-listing-widget value-button)
    (q+:make-qpushbutton)
  (let ((object (object (qui:container slot-listing-widget)))
        (slot (qui:widget-item slot-listing-widget)))
    (setf (q+:text value-button) (if (slot-boundp object slot)
                                     (safe-prin1 (slot-value object slot))
                                     "<UNBOUND>"))
    (setf (q+:style-sheet value-button) "text-align:left;")
    (setf (q+:maximum-height value-button) 50)))

(define-subwidget (slot-listing-widget set-value)
    (make-instance 'inline-button :icon :set :tooltip "Set the slot to a new value."))

(define-subwidget (slot-listing-widget unbind-slot)
    (make-instance 'inline-button :icon :remove :tooltip "Unbind the slot."))

(define-subwidget (slot-listing-widget layout)
    (q+:make-qhboxlayout slot-listing-widget)
  (q+:add-widget layout name)
  (q+:add-widget layout value-button)
  (q+:add-widget layout set-value)
  (q+:add-widget layout unbind-slot)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0))

(define-slot (slot-listing-widget inspect-value) ()
  (declare (connected value-button (clicked)))
  (let ((object (object (qui:container slot-listing-widget)))
        (slot (qui:widget-item slot-listing-widget)))
    (when (slot-boundp object slot)
      (inspect (slot-value object slot)))))

(define-slot (slot-listing-widget set-value) ()
  (declare (connected set-value (clicked)))
  (let* ((object (object (qui:container slot-listing-widget)))
         (slot (qui:widget-item slot-listing-widget)))
    (multiple-value-bind (value got) (safe-input-value slot-listing-widget)
      (when got
        (setf (slot-value object slot) value)
        (setf (q+:text value-button) (safe-prin1 value))))))

(define-slot (slot-listing-widget unbind-slot) ()
  (declare (connected unbind-slot (clicked)))
  (let ((object (object (qui:container slot-listing-widget)))
        (slot (qui:widget-item slot-listing-widget)))
    (slot-makunbound object slot)
    (setf (q+:text value-button) "<UNBOUND>")))
