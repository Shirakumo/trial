(in-package #:org.shirakumo.fraf.trial)

(defclass display (render-loop)
  ((context :initform NIL :accessor context)))

(defmethod initialize-instance :around ((display display) &key)
  (with-cleanup-on-failure (finalize display)
    (call-next-method)))

(defmethod initialize-instance :after ((display display) &key context)
  (etypecase context
    (list
     (setf context (apply #'make-context NIL context))
     (check-type context context))
    (context
     context))
  (setf (context display) context)
  (setf (handler context) display)
  (with-context ((context display))
    (setf +matrix-index+ 0)
    (cache-gl-extensions)
    (prevent-powersave)
    (setup-rendering display)))

(defmethod finalize :around ((display display))
  (unwind-protect (call-next-method)
    (when (context display)
      (finalize (context display))
      (setf (context display) NIL)))
  (restore-powersave))

(defmethod handle (event (display display)))

(defmethod setup-rendering ((display display))
  (reset-matrix (model-matrix))
  (reset-matrix (view-matrix))
  (reset-matrix (projection-matrix))
  (reset-features (feature-table))
  (gl:stencil-mask #xFF)
  (gl:clear-stencil #x00)
  (gl:stencil-func :always 0 #xFF)
  (gl:stencil-op :keep :keep :replace)
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (set-blend-mode :normal)
  (gl:clear-depth 1.0)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:pixel-store :unpack-alignment 1)
  (gl:pixel-store :pack-alignment 1)
  (enable-feature :blend :multisample :cull-face :stencil-test :depth-test :texture-cube-map-seamless)
  (gl:point-size 3.0)
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
      (with-ignored-errors-on-release (:trial.display "Failed to swap buffers!")
        (swap-buffers context)))))

(defmethod render-loop :around ((display display))
  (let ((*context* (context display)))
    (acquire-context *context*)
    (unwind-protect
         (call-next-method)
      (when (context display)
        (quit (context display))
        (release-context (context display))))))

(defmethod width ((display display))
  (width (context display)))

(defmethod height ((display display))
  (height (context display)))

(defclass single-threaded-display (single-threaded-render-loop display)
  ())

(defmethod poll-input :after ((display single-threaded-display))
  (poll-input (context display)))
