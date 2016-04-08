#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-widget editor (QMainWindow)
  ((main :initarg :main :accessor main))
  (:default-initargs
    :main (error "MAIN required.")))

(define-subwidget (editor area) (q+:make-qmdiarea editor)
  (setf (q+:central-widget editor) area))

(define-subwidget (editor game-window) (q+:make-qmdisubwindow editor)
  (setf (q+:widget game-window) main)
  (q+:add-sub-window area game-window))

(define-finalizer (editor teardown)
  (setf (parent main) NIL))

(define-menu (editor File)
  (:item ("Open..." (ctrl o)))
  (:item ("Save" (ctrl s)))
  (:item ("Save As..." (ctrl alt s)))
  (:separator)
  (:item "Exit Editor"
         (q+:close editor)
         (finalize editor))
  (:item "Quit"
         (q+:close editor)
         (finalize editor)
         (q+:close main)))

(define-menu (editor View)
  (:item "Asset Browser")
  (:item "Object Tree")
  (:item "Game View"
         (setf (q+:visible game-window)
               (not (q+:is-visible game-window))))
  (:item "Event Viewer")
  (:item "Console"))
