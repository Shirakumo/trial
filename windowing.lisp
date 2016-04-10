#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *main-window* NIL)

(defun make-gl-format ()
  (let ((format (q+:make-qglformat)))
    (setf (q+:alpha format) T)
    (setf (q+:depth format) T)
    (setf (q+:direct-rendering format) T)
    (setf (q+:double-buffer format) T)
    (setf (q+:rgba format) T)
    (setf (q+:stencil format) T)
    format))

(define-widget main (QGLWidget)
  ((scene :initform (make-instance 'scene) :accessor scene :finalized T)
   (execute-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor execute-queue))
  (:constructor (make-gl-format)))

(define-initializer (main setup)
  (v:info :trial "GENESIS")
  (setf *main-window* main)
  (setf (q+:auto-buffer-swap main) NIL)
  (setf (q+:window-title main) "Trial")
  (setf (q+:minimum-size main) (values 300 200))
  (setf (q+:fixed-size main) (values 1024 768))
  (setf (q+:focus-policy main) (q+:qt.strong-focus))
  (enter (make-instance 'controller) scene))

(define-finalizer (main teardown)
  (v:info :trial "RAPTURE")
  (cl-gamepad:shutdown))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-override (main "initializeGL" initialize-gl) ()
  )

(defclass resize (event)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(define-override (main resize-event) (ev)
  (issue scene 'resize :width (q+:width (q+:size ev)) :height (q+:height (q+:size ev))))

(define-override (main paint-event) (ev))

(define-signal (main launch-editor) ())

(define-slot (main launch-editor) ()
  (declare (connected main (launch-editor)))
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor) main)))

(define-signal (main execute) ())

(define-slot (main execute) ()
  (declare (connected main (execute)))
  (loop for ev across execute-queue
        do (execute ev)
        finally (setf (fill-pointer execute-queue) 0)))

(defun funcall-in-gui (main func &optional bindings)
  (let ((event (make-instance 'execute :func func :bindings bindings)))
    (vector-push-extend event (execute-queue main))
    (values-list
     (loop for result = (result event)
           do (sleep 0.01)
              (case (car result)
                (:failure (error (cdr result)))
                (:success (return (cdr result))))))))

(defun setup-scene (scene)
  (dotimes (i 1000)
    (enter (make-instance 'player :location (vec (- (random 100) 50)
                                                 (- (random 100) 50)
                                                 (- (random 100) 50))) scene))
  )

(defun launch ()
  (v:output-here)
  (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (unwind-protect
       (with-main-window (window 'main #-darwin :main-thread #-darwin NIL))
    (clear-assets)))
