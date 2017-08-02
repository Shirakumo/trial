#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget subject-class-chooser (QDialog)
  ())

(define-initializer (subject-class-chooser setup)
  (setf (q+:window-title subject-class-chooser) "Subject Classes")
  (q+:resize subject-class-chooser 300 500)
  (refresh-instances subject-class-chooser))

(define-subwidget (subject-class-chooser filter)
    (q+:make-qlineedit "subject"))

(define-subwidget (subject-class-chooser clear-filter)
    (q+:make-qpushbutton)
  (setf (q+:icon clear-filter) (q+:standard-icon (q+:style clear-filter)
                                                 (q+:qstyle.sp_dialog-close-button)))
  (setf (q+:tool-tip clear-filter) "Clear the class filter"))

(define-subwidget (subject-class-chooser result-list)
    (make-instance 'class-listing))

(define-subwidget (subject-class-chooser scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget scroller) result-list)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off))
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on)))

(define-subwidget (subject-class-chooser refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (q+:standard-icon (q+:style refresh)
                                            (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip refresh) "Refresh the list of subclasses."))

(define-subwidget (subject-class-chooser inspect)
    (q+:make-qpushbutton)
  (setf (q+:icon inspect) (q+:standard-icon (q+:style inspect)
                                            (q+:qstyle.sp_file-dialog-contents-view)))
  (setf (q+:tool-tip inspect) "Inspect the source code."))

(define-subwidget (subject-class-chooser list-instances)
    (q+:make-qpushbutton)
  (setf (q+:icon list-instances) (q+:standard-icon (q+:style list-instances)
                                                   (q+:qstyle.sp_file-dialog-list-view)))
  (setf (q+:tool-tip list-instances) "List all instances of this class."))

(define-subwidget (subject-class-chooser layout)
    (q+:make-qgridlayout subject-class-chooser)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner filter)
    (q+:add-widget inner clear-filter)
    (q+:add-layout layout inner 0 0 1 3))
  (q+:add-widget layout scroller 1 0 1 3)
  (q+:add-widget layout refresh 2 0 1 1)
  (q+:add-widget layout inspect 2 1 1 1)
  (q+:add-widget layout list-instances 2 2 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (subject-class-chooser refresh refresh-instances) ()
  (declare (connected filter (text-changed string)))
  (declare (connected refresh (clicked)))
  (let ((text (q+:text filter)))
    (qui:clear-layout result-list T)
    (when (string/= text "")
      (let ((class (find-class-fuzzy text)))
        (when class
          (dolist (item (sort (find-class-descendants class) #'string< :key #'class-name))
            (qui:add-item item result-list)))))))

(define-slot (subject-class-chooser clear-filter clear-filter) ()
  (declare (connected clear-filter (clicked)))
  (setf (q+:text filter) ""))

(define-slot (subject-class-chooser inspect) ()
  (declare (connected inspect (clicked)))
  (let ((selected (qui:active-item result-list)))
    (when selected
      (inspect selected))))

(define-slot (subject-class-chooser list-instances) ()
  (declare (connected list-instances (clicked)))
  (let ((selected (qui:active-item result-list)))
    (cond ((not selected))
          ((c2mop:subclassp selected (find-class 'trial:subject))
           (q+:show (make-instance 'subject-chooser :instances-class selected)))
          (T
           (q+:qmessagebox-warning subject-class-chooser "Error" "Can't show instances of a non-subject subclass.")))))

(defun find-class-descendants (base-class)
  (let ((result ()))
    (labels ((r (class)
               (pushnew class result)
               (dolist (sub (c2mop:class-direct-subclasses class))
                 (r sub))))
      (r base-class))
    result))

(defun find-class-fuzzy (text)
  (cond ((find #\: text)
         (let ((package (find-package (string-upcase (subseq text 0 (position #\: text))))))
           (when package
             (let ((symbol (find-symbol (string-upcase (subseq text (1+ (position #\: text)))) package)))
               (and symbol (find-class symbol NIL))))))
        (T
         (do-all-symbols (symbol)
           (when (and (string-equal symbol text)
                      (find-class symbol NIL))
             (return (find-class symbol NIL)))))))

(define-widget class-listing (QWidget qui:listing)
  ()
  (:default-initargs :draggable NIL))

(defmethod qui:coerce-item ((class class) (listing class-listing))
  (make-instance 'class-listing-widget :item class :container listing))

(define-widget class-listing-widget (QLabel qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-initializer (class-listing-widget setup)
  (let ((class (qui:widget-item class-listing-widget)))
    (setf (q+:text class-listing-widget) (format NIL "~(~s~)" (class-name class)))
    (setf (q+:tool-tip class-listing-widget) (or (documentation class T) ""))))

(define-override (class-listing-widget mouse-double-click-event) (ev)
  (q+:show (make-instance 'subject-chooser :instances-class (qui:widget-item class-listing-widget))))
