#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass display (render-loop)
  ((context :initform NIL :accessor context)
   (clear-color :initarg :clear-color :accessor clear-color))
  (:default-initargs
   :clear-color (vec 0.2 0.3 0.3)))

(defmethod initialize-instance :around ((display display) &key)
  (with-cleanup-on-failure (finalize display)
    (call-next-method)))

(defmethod initialize-instance :after ((display display) &rest initargs &key context)
  (etypecase context
    (list
     (setf context (apply #'make-context NIL context)))
    (context
     context))
  (setf (context display) context)
  (setf (handler context) display)
  (setf +matrix-index+ 0)
  #-arm64 (prevent-powersave)
  (with-context ((context display))
    (setup-rendering display)))

(defmethod finalize :around ((display display))
  (unwind-protect (call-next-method)
    (when (context display)
      (finalize (context display))
      (setf (context display) NIL)))
  #-arm64 (restore-powersave))

(defmethod handle (event (display display)))

(defmethod setup-rendering ((display display))
  (reset-matrix (model-matrix))
  (reset-matrix (view-matrix))
  (reset-matrix (projection-matrix))
  (reset-attributes (attribute-table))
  (gl:stencil-mask #xFF)
  (gl:clear-stencil #x00)
  (gl:stencil-op :keep :keep :keep)
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  #-arm64 (gl:clear-depth 1.0)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:pixel-store :unpack-alignment 1)
  (with-vec (r g b a) (clear-color display)
    (gl:clear-color r g b a))
  (enable-feature :blend :multisample :cull-face :stencil-test :depth-test)
  (when-gl-extension :gl-arb-depth-clamp
    (enable-feature :depth-clamp)))

(defmethod update :after ((display display) tt dt fc)
  (declare (type double-float tt))
  (ping-powersave tt))

(defgeneric poll-input (target))

(defmethod poll-input ((target display)))

(defmethod render :around (source (target display))
  ;; Potentially release context every time to allow
  ;; other threads to grab it.
  (let ((context (context target)))
    (with-context (context :reentrant T)
      (call-next-method)
      (swap-buffers context))))

(defmethod render-loop :around ((display display))
  (let ((*context* (context display)))
    (unwind-protect
         (call-next-method)
      (when (context display)
        (release-context (context display))))))

(defmethod width ((display display))
  (width (context display)))

(defmethod height ((display display))
  (height (context display)))
