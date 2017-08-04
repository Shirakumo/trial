#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget array-inspector (QDialog inspector)
  ((object)))

(define-initializer (array-inspector setup)
  (setf (q+:window-title array-inspector) "Array Inspector")
  (q+:resize array-inspector 300 600)
  (refresh-instances array-inspector))

(define-subwidget (array-inspector array-info)
    (q+:make-qformlayout)
  (setf (q+:margin array-info) 5)
  (setf (q+:spacing array-info) 0))

(define-subwidget (array-inspector entries)
    (make-instance 'array-listing :inspector array-inspector))

(define-subwidget (array-inspector scroller)
    (q+:make-qscrollarea)
  (setf (q+:widget scroller) entries)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off))
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on)))

(define-subwidget (array-inspector add)
    (q+:make-qpushbutton)
  (setf (q+:icon add) (q+:standard-icon (q+:style add)
                                        (q+:qstyle.sp_file-dialog-new-folder)))
  (setf (q+:tool-tip add) "Add an array element."))

(define-subwidget (array-inspector adjust)
    (q+:make-qpushbutton)
  (setf (q+:icon adjust) (q+:standard-icon (q+:style adjust)
                                           (q+:qstyle.sp_dialog-close-button)))
  (setf (q+:tool-tip adjust) "Adjust the array."))

(define-subwidget (array-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (q+:standard-icon (q+:style refresh)
                                            (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip refresh) "Refresh the list of entries."))

(define-subwidget (array-inspector layout)
    (q+:make-qgridlayout array-inspector)
  (q+:add-layout layout array-info 0 0 1 3)
  (q+:add-widget layout scroller 1 0 1 3)
  (q+:add-widget layout refresh 2 0 1 1)
  (q+:add-widget layout add 2 1 1 1)
  (q+:add-widget layout adjust 2 2 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (array-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (sweep-layout array-info)
  (flet ((add-row (name value)
           (let ((label (q+:make-qlabel (prin1-to-string value))))
             (q+:add-row array-info name label)
             (setf (q+:alignment label) (q+:qt.align-right)))))
    (add-row "Length" (if (typep object 'vector) (length object) (array-total-size object)))
    (add-row "Dimensions" (array-dimensions object))
    (add-row "Element-type" (array-element-type object)))
  (qui:clear-layout entries T)
  (bt:make-thread
   (lambda ()
     (if (typep object 'vector)
         (loop for thing across object
               for i from 0
               do (add-item i entries)
                  (sleep 0.01))
         (loop for i from 0 below (array-total-size object)
               do (add-item i entries)
                  (sleep 0.01))))))

(define-slot (array-inspector add-item) ((item qobject))
  (declare (connected array-inspector (add-item qobject)))
  (when (typep item 'signal-carrier)
    (qui:add-item (object item) entries)))

(define-slot (array-inspector adjust) ()
  (declare (connected adjust (clicked)))
  (if (adjustable-array-p object)
      (cffi:with-foreign-object (ok :bool)
        (let ((value (q+:qinputdialog-get-int array-inspector "Enter new array size"
                                              "Enter the new array size:" (array-total-size object)
                                              0 (1- (expt 2 31)) 1 ok)))
          (when (cffi:mem-ref ok :bool)
            (if (array-has-fill-pointer-p object)
                (adjust-array object value :fill-pointer (min value (fill-pointer object)))
                (adjust-array object value))
            (refresh-instances array-inspector))))
      (q+:qmessagebox-critical array-inspector "Error adjusting array"
                               "The array is not adjustable!")))

(define-slot (array-inspector add) ()
  (declare (connected add (clicked)))
  (cond ((not (array-has-fill-pointer-p object))
         (q+:qmessagebox-critical array-inspector "Error adding a new element"
                                  "Can't add a new element: the array has no fill pointer!"))
        ((and (not (adjustable-array-p object))
              (= (fill-pointer object) (array-total-size object)))
         (q+:qmessagebox-critical array-inspector "Error adding a new element"
                                  "Can't add a new element: the array is not adjustable and already full!"))
        (T (multiple-value-bind (value got) (safe-input-value array-inspector)
             (when got
               (if (adjustable-array-p object)
                   (vector-push-extend value object)
                   (vector-push value object))
               (refresh-instances array-inspector))))))

(define-widget array-listing (QWidget qui:listing)
  ((inspector :initarg :inspector :accessor inspector)))

(defmethod qui:coerce-item (index (listing array-listing))
  (make-instance 'array-listing-widget :item index :container listing))

(define-widget array-listing-widget (QWidget qui:listing-item)
  ()
  (:default-initargs :draw-item NIL))

(define-subwidget (array-listing-widget index)
    (q+:make-qlabel (safe-princ (qui:widget-item array-listing-widget)))
  (setf (q+:fixed-width index) 50))

(define-subwidget (array-listing-widget value-button)
    (let ((object (object (inspector (qui:container array-listing-widget))))
          (key (qui:widget-item array-listing-widget)))
      (q+:make-qpushbutton (safe-prin1 (aref object key)))))

(define-subwidget (array-listing-widget set-value)
    (q+:make-qpushbutton)
  (setf (q+:icon set-value) (q+:standard-icon (q+:style set-value)
                                              (q+:qstyle.sp_arrow-left)))
  (setf (q+:tool-tip set-value) "Set the element to a new value.")
  (setf (q+:fixed-width set-value) 40))

(define-subwidget (array-listing-widget remove-entry)
    (q+:make-qpushbutton)
  (setf (q+:icon remove-entry) (q+:standard-icon (q+:style remove-entry)
                                                 (q+:qstyle.sp_dialog-close-button)))
  (setf (q+:tool-tip remove-entry) "Remove the element.")
  (setf (q+:fixed-width remove-entry) 40))

(define-subwidget (array-listing-widget layout)
    (q+:make-qhboxlayout array-listing-widget)
  (q+:add-widget layout index)
  (q+:add-widget layout value-button)
  (q+:add-widget layout set-value)
  (q+:add-widget layout remove-entry)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0))

(define-slot (array-listing-widget inspect-value) ()
  (declare (connected value-button (clicked)))
  (let ((object (object (inspector (qui:container array-listing-widget))))
        (index (qui:widget-item array-listing-widget)))
    (inspect (row-major-aref object index))))

(define-slot (array-listing-widget set-value) ()
  (declare (connected set-value (clicked)))
  (let* ((object (object (inspector (qui:container array-listing-widget))))
         (index (qui:widget-item array-listing-widget)))
    (multiple-value-bind (value got) (safe-input-value array-listing-widget)
      (when got
        (setf (row-major-aref object index) value)
        (setf (q+:text value-button) (safe-prin1 value))))))

(define-slot (array-listing-widget remove-entry) ()
  (declare (connected remove-entry (clicked)))
  (let ((object (object (inspector (qui:container array-listing-widget))))
        (index (qui:widget-item array-listing-widget)))
    (cond ((adjustable-array-p object)
           (array-utils:vector-pop-position object index)
           (refresh-instances (inspector (qui:container array-listing-widget))))
          ((typep NIL (array-element-type object))
           (setf (row-major-aref object index) NIL)
           (setf (q+:text value-button) (safe-prin1 NIL)))
          (T
           (q+:qmessagebox-critical array-listing-widget "Failed to remove entry"
                                    "Cannot remove this entry: the array is not a vector and its element-type does not allow NIL for values.")))))
