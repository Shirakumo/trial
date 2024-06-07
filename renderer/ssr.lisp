(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass ssr-render-pass (simple-post-effect-pass standard-environment-pass)
  ((previous-depth :port-type input :texspec (:internal-format :depth-component) :accessor previous-depth))
  (:shader-file (trial "renderer/ssr-pass.glsl")))
