#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget symbol-inspector (QDialog inspector)
  ((object)))

(define-initializer (symbol-inspector setup)
  (setf (q+:window-title symbol-inspector) (format NIL "Symbol Inspector for ~s" object))
  (q+:resize symbol-inspector 400 200)
  (refresh-instances symbol-inspector))

(defmacro define-symbol-attr (name &key reader accessor bound-test unbinder)
  (let ((set-name (intern (format NIL "~a-~a" 'set name)))
        (unbind-name (intern (format NIL "~a-~a" 'unbind name))))
    `(progn
       (define-subwidget (symbol-inspector ,name)
           ,(if reader
                '(q+:make-qpushbutton)
                '(q+:make-qlabel)))

       ,@(when reader
           `((define-slot (symbol-inspector ,name) ()
               (declare (connected ,name (clicked)))
               ,(if bound-test
                    `(when (,bound-test object)
                       (inspect (,reader object)))
                    `(inspect (,reader object))))))
       
       ,@(when accessor
           `((define-subwidget (symbol-inspector ,set-name)
                 (make-instance 'inline-button :icon :set :tooltip ,(format NIL "Set the ~(~a~)." unbinder)))
             
             (define-slot (symbol-inspector ,set-name) ()
               (declare (connected ,set-name (clicked)))
               (multiple-value-bind (value got) (safe-input-value symbol-inspector)
                 (when got
                   (setf (,accessor object) value)
                   (refresh-instances symbol-inspector))))))
       
       ,@(when unbinder
           `((define-subwidget (symbol-inspector ,unbind-name)
                 (make-instance 'inline-button :icon :remove :tooltip ,(format NIL "Unbind the ~(~a~)." unbinder)))

             (define-slot (symbol-inspector ,unbind-name) ()
               (declare (connected ,unbind-name (clicked)))
               (,unbinder object)
               (refresh-instances symbol-inspector)))))))

(define-symbol-attr name)

(define-symbol-attr package
  :reader symbol-package)

(define-symbol-attr value
  :reader symbol-value
  :accessor symbol-value
  :bound-test boundp
  :unbinder makunbound)

(define-symbol-attr function
  :reader symbol-function
  :accessor symbol-function
  :bound-test fboundp
  :unbinder fmakunbound)

(define-symbol-attr plist
  :reader symbol-plist
  :accessor symbol-plist)

(define-subwidget (symbol-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (q+:standard-icon (q+:style refresh)
                                            (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip refresh) "Refresh the symbol."))

(define-subwidget (symbol-inspector layout)
    (q+:make-qgridlayout symbol-inspector)
  (q+:add-widget layout (q+:make-qlabel "Name") 0 0 1 1)
  (q+:add-widget layout name 0 1 1 1)
  (q+:add-widget layout (q+:make-qlabel "Package") 1 0 1 1)
  (q+:add-widget layout package 1 1 1 1)
  (q+:add-widget layout (q+:make-qlabel "Value") 2 0 1 1)
  (q+:add-widget layout value 2 1 1 1)
  (q+:add-widget layout set-value 2 2 1 1)
  (q+:add-widget layout unbind-value 2 3 1 1)
  (q+:add-widget layout (q+:make-qlabel "Function") 3 0 1 1)
  (q+:add-widget layout function 3 1 1 1)
  (q+:add-widget layout set-function 3 2 1 1)
  (q+:add-widget layout unbind-function 3 3 1 1)
  (q+:add-widget layout (q+:make-qlabel "Plist") 4 0 1 1)
  (q+:add-widget layout plist 4 1 1 1)
  (q+:add-widget layout set-plist 4 2 1 1)
  (q+:add-widget layout refresh 5 0 1 4)
  (setf (q+:spacing layout) 0))

(define-slot (symbol-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (setf (q+:text name) (symbol-name object))
  (setf (q+:text package) (if (symbol-package object)
                              (package-name (symbol-package object))
                              ""))
  (setf (q+:text value) (if (boundp object)
                            (safe-prin1 (symbol-value object))
                            "<UNBOUND>"))
  (setf (q+:text function) (if (fboundp object)
                               (safe-princ (symbol-function object))
                               "<UNBOUND>"))
  (setf (q+:text plist) (safe-prin1 (symbol-plist object))))
