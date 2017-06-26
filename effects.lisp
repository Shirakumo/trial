#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-pool effects
  :base 'trial)

(define-shader-pass render-pass (per-object-pass)
  ((color :port-type output :attachment :color-attachment0)
   (depth :port-type output :attachment :depth-attachment)))

(define-shader-pass msaa-pass (render-pass multisampled-per-object-pass)
  ())

(define-shader-pass simple-post-effect-pass (post-effect-pass)
  ((previous-pass :port-type input)
   (color :port-type output)))

(define-shader-pass copy-pass (simple-post-effect-pass)
  ())

(define-class-shader (copy-pass :fragment-shader)
  "
uniform sampler2D previous_pass;
in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(previous_pass, tex_coord);
}")

(define-shader-pass negative-pass (simple-post-effect-pass)
  ())

(define-class-shader (negative-pass :fragment-shader)
  '(effects #p"negative.frag"))

(define-shader-pass grayscale-pass (simple-post-effect-pass)
  ())

(define-class-shader (grayscale-pass :fragment-shader)
  '(effects #p"gray-filter.frag"))

(define-shader-pass box-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (box-blur-pass :fragment-shader)
  '(effects #p"box-blur.frag"))

(define-shader-pass sobel-pass (simple-post-effect-pass)
  ())

(define-class-shader (sobel-pass :fragment-shader)
  '(effects #p"sobel.frag"))

(define-shader-pass gaussian-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (gaussian-blur-pass :fragment-shader)
  '(effects #p"gaussian.frag"))

(define-shader-pass radial-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (radial-blur-pass :fragment-shader)
  '(effects #p"radial-blur.frag"))

(define-shader-pass fxaa-pass (simple-post-effect-pass)
  ())

(define-class-shader (fxaa-pass :fragment-shader)
  '(effects #p"fxaa.frag"))

(define-shader-pass blend-pass (post-effect-pass)
  ((a-pass :port-type input)
   (b-pass :port-type input)
   (color :port-type output)))

(define-class-shader (blend-pass :fragment-shader)
  '(effects #p"blend.frag"))

(define-shader-pass high-pass-filter (simple-post-effect-pass)
  ())

(define-class-shader (high-pass-filter :fragment-shader)
  '(effects #p"high-pass-filter.frag"))

(define-shader-pass low-pass-filter (simple-post-effect-pass)
  ())

(define-class-shader (low-pass-filter :fragment-shader)
  '(effects #p"low-pass-filter.frag"))

(define-shader-pass chromatic-aberration-filter (simple-post-effect-pass)
  ())

(define-class-shader (chromatic-aberration-filter :fragment-shader)
  '(effects #p"aberration.frag"))

(define-shader-pass black-render-pass (render-pass)
  ((color :port-type output)))

(define-class-shader (black-render-pass :fragment-shader)
  "out vec4 color;

void main(){
  color = vec4(0, 0, 0, 1);
}")

(define-shader-pass light-scatter-pass (post-effect-pass)
  ((previous-pass :port-type input)
   (black-render-pass :port-type input)
   (color :port-type output)))

(define-class-shader (light-scatter-pass :fragment-shader)
  '(effects #p"light-scatter.frag"))

(define-shader-pass depth-peel-pass (per-object-pass)
  ((layers :initarg :layers :accessor layers)
   (previous-depth :port-type buffer :attachment :depth-attachment :accessor previous-depth)
   (color :port-type output :attachment :color-attachment0 :accessor color)
   (depth :port-type output :attachment :depth-attachment :accessor depth))
  (:default-initargs :layers 8))

(defmethod paint ((pass depth-peel-pass) target)
  (gl:blend-equation :func-add)
  (gl:blend-func-separate :dst-alpha :one
                          :zero :one-minus-src-alpha)
  (dotimes (i (layers pass))
    (call-next-method)
    ;; Swap textures.
    (rotatef (previous-depth pass) (depth pass))
    (%gl:framebuffer-texture :framebuffer :depth-attachment (resource (depth pass)) 0)))

(define-class-shader (depth-peel-pass :fragment-shader 100)
  "uniform sampler2D previous_depth;

void main(){
  float depth = gl_FragCoord.z;
  float previous_depth = texture(previous_depth, gl_FragCoord.xy).r;

  // This may seem confusing, but 1.0 is \"far\" and 0.0 is \"near\".
  if(depth <= previous_depth){
    discard;
  }
}")
