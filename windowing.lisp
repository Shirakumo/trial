#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QGLWidget context)
  ((scene :initform (make-instance 'scene) :accessor scene :finalized NIL)
   (controller :accessor controller :finalized NIL)
   (execute-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor execute-queue)))

(defmethod construct ((main main))
  (new main (context main)))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-initializer (main setup)
  (v:info :trial "GENESIS")
  (setf *main* main)
  (setf (q+:updates-enabled main) NIL)
  (setf (q+:auto-buffer-swap main) NIL)
  (setf (q+:window-title main) "Trial")
  (setf (q+:minimum-size main) (values 300 200))
  (setf (q+:fixed-size main) (values 1024 768))
  (setf (q+:focus-policy main) (q+:qt.strong-focus))
  (setf scene (make-instance 'scene))
  (release-context main)
  (setf controller (make-instance 'controller))
  (enter controller scene))

(define-finalizer (main teardown)
  (v:info :trial "RAPTURE")
  (cl-gamepad:shutdown)
  (finalize controller)
  (acquire-context main :reacquire T :force T)
  (finalize scene)
  (dolist (pool (pools))
    (mapc #'offload (assets pool))))

;;; REASON FOR THE FOLLOWING TWO OVERRIDES:
;; The rendering in this engine works as follows.
;; There is a main thread that controls the Qt windows and a separate thread that handles
;; the game updating and GL rendering. Now, OpenGL has a context, that can only ever be
;; used from one thread at once. If we want to draw from another thread, we first need to
;; make the context current to that thread. As such, in order to start drawing in our
;; rendering thread, we need to make the context current there. Unfortunately for us, the
;; QGLWidget offers some convenience methods called initializeGL, resizeGL, and paintGL,
;; which are always called from the main thread, and /automatically/ acquire the context.
;; As such, if one of these methods is called by Qt, it fucks up our rendering thread as
;; it steals the GL context out from under its feet. Since we don't need these methods
;; and they're actually actively harmful, we need to prevent Qt from ever calling them.
;;
;; That's why the following two overrides exist. The resize-event merely issues a new
;; event to the scene, which will then trigger the actual resizing in the controller's
;; handler. The paint-event override does absolutely nothing, which is fine because we
;; do all the drawing and buffer swapping in the rendering thread anyway, and doing this
;; prevents the calling of paintGL.

(defclass resize (event)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(define-override (main resize-event) (ev)
  (issue scene 'resize :width (q+:width (q+:size ev)) :height (q+:height (q+:size ev))))

(define-override (main paint-event) (ev))

;; We need to do this with a signal because the editor has to be launched through the
;; GUI thread.

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

(defun funcall-in-gui (main func &key bindings (want-results T))
  (let ((event (make-instance 'execute :func func :bindings bindings)))
    (vector-push-extend event (execute-queue main))
    (when want-results
      (values-list
       (loop for result = (result event)
             do (sleep 0.01)
                (case (car result)
                  (:failure (error (cdr result)))
                  (:success (return (cdr result)))))))))

(defun launch ()
  (v:output-here)
  (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (with-main-window (window 'main #-darwin :main-thread #-darwin NIL)))

(defmethod width ((object qobject))
  (q+:width object))

(defmethod height ((object qobject))
  (q+:height object))

(defmethod (setf parent) :after (parent (main main))
  (issue (scene main) 'acquire-context))
