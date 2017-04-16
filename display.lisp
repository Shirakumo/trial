#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-widget display (QGLWidget context renderable)
  ((clear-color :initarg :clear-color :initform (vec 0.2 0.3 0.3) :accessor clear-color)))

(defmethod handle (event (display display)))

(define-initializer (display %display-setup)
  (v:info :trial.display "~a is launching..." display)
  (setf (q+:auto-fill-background display) NIL)
  (setf (q+:auto-buffer-swap display) NIL)
  (setup-rendering display)
  (release-context display))

;;; REASON FOR THE OVERRIDES:
;; The rendering in this engine works as follows.
;; There is a main thread that controls the Qt windows and a separate thread that handles
;; the display updating and GL rendering. Now, OpenGL has a context, that can only ever be
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

(define-subwidget (display resize-timer) (q+:make-qtimer display)
  (setf (q+:single-shot resize-timer) T))

(define-override (display resize-event) (ev)
  (q+:start resize-timer 100))

(define-slot (display resize-timer) ()
  (declare (connected resize-timer (timeout)))
  (handle (make-instance 'resize :width (q+:width display) :height (q+:height display))
          display))

(define-override (display paint-event) (ev))

(defmethod setup-rendering ((display display))
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-depth 1.0)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:hint :line-smooth-hint :nicest)
  (enable :blend :multisample :line-smooth :depth-test :depth-clamp))

(defmethod paint (source (target display)))

(defmethod render (source (target display))
  (paint source target))

(defmethod render :around (source (target display))
  ;; Potentially release context every time to allow
  ;; other threads to grab it.
  (with-context (target :reentrant T)
    (let ((c (clear-color target)))
      (gl:clear-color (vx c) (vy c) (vz c) (if (vec4-p c) (vw c) 1.0)))
    (gl:clear :color-buffer :depth-buffer)
    (call-next-method)
    (q+:swap-buffers target)))
