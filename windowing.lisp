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
  ((scene :initform (make-instance 'scene) :accessor scene :finalized T))
  (:constructor (make-gl-format)))

(define-initializer (main setup)
  (v:info :trial "GENESIS")
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:auto-buffer-swap main) NIL)
  (setf (q+:window-title main) "Trial")
  (setf (q+:minimum-size main) (values 300 200))
  (setf (q+:focus-policy main) (q+:qt.strong-focus)))

(define-finalizer (main teardown)
  (v:info :trial "RAPTURE")
  (cl-gamepad:shutdown))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-override (main "initializeGL" initialize-gl) ()
  (q+:qgl-clear-color main background)
  (gl:enable :depth-test :blend :cull-face :texture-2d)
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:clear-depth 1.0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :smooth)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:hint :perspective-correction-hint :nicest)
  (cl-gamepad:init)
  (setup-scene scene))

(defclass resize (event)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(define-override (main "resizeGL" resize-gl) (width height)
  (issue scene 'resize :width width :height height))

(define-signal (main launch-editor) ())

(define-slot (main launch-editor) ()
  (declare (connected main (launch-editor)))
  (q+:show (make-instance 'editor :main main)))

(defun setup-scene (scene)
  (enter (make-instance 'player) scene)
  (enter (make-instance 'controller) scene))

(defun launch ()
  (v:output-here)
  (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (unwind-protect
       (with-main-window (window 'main #-darwin :main-thread #-darwin NIL))
    (clear-assets)))
