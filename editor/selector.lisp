#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.editor)
(in-readtable :qtools)

(define-widget selector (QDialog)
  ((scene :initarg :scene :accessor scene)
   (buffer :initform NIL :accessor buffer))
  (:default-initargs :scene (error "SCENE required.")))

(define-initializer (selector setup)
  (setf (q+:window-title selector) "Selector")
  (q+:resize selector 500 600)
  (setf (buffer selector) (make-instance 'trial::selection-buffer :width 800 :height 600 :scene (scene selector)))
  (trial:issue (scene selector) 'trial:load-request :asset (buffer selector) :action 'trial:load)
  (trial:add-handler selector (scene selector)))

(define-finalizer (selector teardown)
  ;; FIXME: leak (buffer selector)
  (trial:remove-handler selector (scene selector)))

(define-subwidget (selector empty-text)
    (q+:make-qlabel "Select a subject in the game window.")
  (setf (q+:alignment empty-text) (q+:qt.align-center)))

(define-subwidget (selector inspector)
    (make-instance 'object-inspector :object NIL))

(define-subwidget (selector layout)
    (q+:make-qhboxlayout selector)
  (q+:add-widget layout empty-text)
  (q+:add-widget layout inspector)
  (q+:hide inspector)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0))

(define-signal (selector show-object) (qobject))

(define-slot (selector show-object) ((object qobject))
  (declare (connected selector (show-object qobject)))
  (let ((object (object object)))
    (cond (object
           (setf (object inspector) object)
           (q+:show inspector) (q+:hide empty-text))
          (T
           (q+:hide inspector) (q+:show empty-text)))))

(defmethod trial:handle (event (selector selector))
  (trial:handle event (buffer selector)))

(defmethod trial:handle ((ev trial:reload-scene) (selector selector))
  ;; FIXME: leak (buffer selector)
  )

(defmethod trial:handle ((ev trial:mouse-release) (selector selector))
  (trial:paint (buffer selector) (buffer selector))
  (let ((object (trial::object-at-point (trial:pos ev) (buffer selector))))
    (signal! selector (show-object qobject) (make-instance 'signal-carrier :object object))))
