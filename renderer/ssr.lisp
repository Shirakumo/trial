(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass ssr-pbr-render-pass (z-prepass-standard-render-pass pbr-render-pass)
  ((previous-pass :port-type static-input :accessor previous-pass))
  (:shader-file (trial "standard-render-pbr-ssr.glsl")))

(defmethod render :before ((pass ssr-pbr-render-pass) thing)
  (rotatef (gl-name (previous-pass pass)) (gl-name (color pass)))
  (let ((framebuffer (framebuffer pass)))
    (gl:bind-framebuffer :framebuffer (gl-name framebuffer))
    (%gl:framebuffer-texture :framebuffer :color-attachment0 (gl-name (color pass)) 0)))
