#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-widget display (QGLWidget context)
  ((scene :initform (make-instance 'scene) :accessor scene)
   (controller :initform (make-instance 'controller :display NIL) :accessor controller)))

(define-initializer (display setup)
  (v:info :trial.display "~a is launching..." display)
  (setf (display controller) display)
  (setup-rendering display)
  (setup-scene display)
  (start scene)
  (release-context display)
  ;; Launch the controller
  (enter controller scene))

(define-finalizer (display teardown)
  (v:info :trial.display "~a is tearing down..." display)
  (finalize controller)
  (acquire-context display :force T)
  (finalize scene))

;;; REASON FOR THE FOLLOWING TWO OVERRIDES:
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

(define-override (display resize-event) (ev)
  (issue scene 'resize :width (q+:width (q+:size ev)) :height (q+:height (q+:size ev))))

(define-override (display paint-event) (ev))

(defmethod setup-rendering ((display display))
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:clear-depth 1.0)
  (gl:alpha-func :greater 0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :smooth)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:hint :line-smooth-hint :nicest)
  (gl:hint :polygon-smooth-hint :nicest))

(defmethod render ((display source) (display target))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :blend :cull-face :texture-2d :multisample
             :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test)
  (with-pushed-matrix
    (paint (scene source) target))
  (gl:load-identity))

(defmethod render-hud ((display source) (display target))
  (gl:with-pushed-matrix* (:projection)
    (gl:load-identity)
    (gl:ortho 0 (q+:width target) (q+:height target) 0 -1 10)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:disable :cull-face)
    (gl:clear :depth-buffer)

    (with-pushed-matrix
      (render-hud (controller display) display)))
  (gl:matrix-mode :modelview))

(defmethod setup-scene ((display display)))

(defmethod setup-scene :around ((display display))
  (with-simple-restart (continue "Skip loading the rest of the scene and hope for the best.")
    (call-next-method)))
