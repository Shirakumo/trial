#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget function-inspector (QDialog inspector)
  ((object)))

(define-initializer (function-inspector setup)
  (setf (q+:window-title function-inspector) (format NIL "Inspecting ~a" (safe-princ object)))
  (q+:resize function-inspector 500 600)
  (refresh-instances function-inspector))

(define-subwidget (function-inspector function-info)
    (q+:make-qformlayout))

(define-subwidget (function-inspector editor)
    (q+:make-qplaintextedit))

(define-subwidget (function-inspector font) (q+:make-qfont "Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font editor) font))

(define-subwidget (function-inspector refresh)
    (q+:make-qpushbutton)
  (setf (q+:icon refresh) (q+:standard-icon (q+:style refresh)
                                            (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip refresh) "Refresh the function."))

(define-subwidget (function-inspector recompile)
    (q+:make-qpushbutton)
  (setf (q+:icon recompile) (q+:standard-icon (q+:style recompile)
                                              (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip recompile) "Recompile the function."))

(define-subwidget (function-inspector unbind)
    (q+:make-qpushbutton)
  (setf (q+:icon unbind) (q+:standard-icon (q+:style unbind)
                                           (q+:qstyle.sp_browser-reload)))
  (setf (q+:tool-tip unbind) "Unbind the function."))

(define-subwidget (function-inspector layout)
    (q+:make-qgridlayout function-inspector)
  (q+:add-layout layout function-info 0 0 1 3)
  (q+:add-widget layout editor 1 0 1 3)
  (q+:add-widget layout refresh 2 0 1 1)
  (q+:add-widget layout recompile 2 1 1 1)
  (q+:add-widget layout unbind 2 2 1 1)
  (setf (q+:spacing layout) 0))

(define-slot (function-inspector refresh refresh-instances) ()
  (declare (connected refresh (clicked)))
  (let* ((location (cdr (swank:find-definition-for-thing
                         (if (symbolp object)
                             (or (macro-function object)
                                 (fdefinition object))
                             object))))
         (file (second (assoc :file location)))
         (position (second (assoc :position location))))
    (when location
      (setf (q+:plain-text editor)
            (alexandria:read-file-into-string file))
      (let ((cursor (q+:text-cursor editor)))
        (setf (q+:position cursor) position)
        (setf (q+:text-cursor editor) cursor)
        (q+:ensure-cursor-visible editor)))))

(define-slot (function-inspector recompile) ()
  (declare (connected recompile (clicked)))
  )

(define-slot (function-inspector unbind) ()
  (declare (connected unbind (clicked)))
  )
