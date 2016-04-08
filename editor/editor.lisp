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

(define-subwidget (editor game-view) (q+:make-qmdisubwindow editor)
  (setf (q+:widget game-view) main)
  (setf (q+:window-title game-view) "Game View")
  (q+:add-sub-window area game-view))

(define-initializer (editor setup)
  (setf (q+:window-title editor) "Trial Editor"))

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
         (setf (q+:visible game-view) (not (q+:is-visible game-view))))
  (:item "Event Viewer")
  (:item "Console"))
