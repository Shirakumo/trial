(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass ssr-pbr-render-pass (z-prepass-standard-render-pass pbr-render-pass)
  ((previous-pass :port-type static-input :accessor previous-pass))
  (:shader-file (trial "standard-render-pbr-ssr.glsl")))

(defmethod render :after ((pass ssr-pbr-render-pass) thing)
  (rotatef (gl-name (previous pass)) (gl-name (color pass)))
  (%gl:framebuffer-texture :framebuffer :color-attachment0 (gl-name (color pass)) 0))
