#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget subject-chooser (QDialog)
  ((instances-class :initarg :instances-class :accessor instances-class))
  (:default-initargs :instances-class (error "CLASS required.")))

(define-initializer (subject-chooser setup)
  (setf (q+:window-title subject-chooser) (format NIL "Instances of ~s"
                                                   (class-name (instances-class subject-chooser))))
  (q+:resize subject-chooser 300 400)
  (refresh-instances subject-chooser))

(define-subwidget (subject-chooser filter)
    (q+:make-qlineedit)
  (setf (q+:placeholder-text filter) "Filter..."))

(define-subwidget (subject-chooser clear-filter)
    (q+:make-qpushbutton)
  (setf (q+:focus-policy clear-filter) (q+:qt.no-focus))
  (setf (q+:icon clear-filter) (icon :close))
  (setf (q+:tool-tip clear-filter) "Clear the instance filter"))

(define-subwidget (subject-chooser result-list)
    (make-instance 'instance-listing))

(define-subwidget (subject-chooser scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget scroller) result-list)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off))
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on)))

(define-subwidget (subject-chooser refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (icon :refresh))
  (setf (q+:tool-tip refresh) "Refresh the list of instances."))

(define-subwidget (subject-chooser inspect)
    (q+:make-qpushbutton)
  (setf (q+:icon inspect) (icon :inspect))
  (setf (q+:tool-tip inspect) "Inspect the source code."))

(define-subwidget (subject-chooser layout)
    (q+:make-qgridlayout subject-chooser)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner filter)
    (q+:add-widget inner clear-filter)
    (q+:add-layout layout inner 0 0 1 2))
  (q+:add-widget layout scroller 1 0 1 2)
  (q+:add-widget layout refresh 2 0 1 1)
  (q+:add-widget layout inspect 2 1 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (subject-chooser refresh refresh-instances) ()
  (declare (connected filter (text-changed string)))
  (declare (connected refresh (clicked)))
  (qui:clear-layout result-list T)
  (dolist (item (sort (remove-if-not (lambda (a) (search (q+:text filter) (string (flare:name a)) :test #'char-equal))
                                     (find-subject-instances (instances-class subject-chooser)))
                      #'string< :key #'princ-to-string))
    (qui:add-item item result-list)))

(define-slot (subject-chooser inspect) ()
  (declare (connected inspect (clicked)))
  (let ((selected (qui:active-item result-list)))
    (when selected
      (inspect selected))))

(defun find-subject-instances (base-class)
  ()
  ;;; FIXME: Instances are no longer tracked by Trial!
  ;; (let ((result ()))
  ;;   (labels ((r (class)
  ;;              (loop for ptr in (trial:instances class)
  ;;                    for val = (tg:weak-pointer-value ptr)
  ;;                    when val do (push val result))
  ;;              (dolist (sub (c2mop:class-direct-subclasses class))
  ;;                (r sub))))
  ;;     (r base-class))
  ;;   result)
  )

(define-widget instance-listing (QWidget qui:listing)
  ()
  (:default-initargs :draggable NIL))

(defmethod qui:coerce-item (instance (listing instance-listing))
  (make-instance 'instance-listing-widget :item instance :container listing))

(define-widget instance-listing-widget (QLabel qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-initializer (instance-listing-widget setup)
  (let ((instance (qui:widget-item instance-listing-widget)))
    (setf (q+:text instance-listing-widget) (typecase instance
                                              (flare:unit (format NIL "~(~s~)" (flare:name instance)))
                                              (T (princ-to-string instance))))))

(define-override (instance-listing-widget mouse-double-click-event) (ev)
  (inspect (qui:widget-item instance-listing-widget)))
