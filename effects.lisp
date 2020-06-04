#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-pool effects
  :base 'trial)

(define-shader-pass render-pass (scene-pass per-object-pass)
  ((color :port-type output :attachment :color-attachment0)
   (depth :port-type output :attachment :depth-stencil-attachment)))

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
  (pool-path 'effects #p"negative.frag"))

(define-shader-pass grayscale-pass (simple-post-effect-pass)
  ())

(define-class-shader (grayscale-pass :fragment-shader)
  (pool-path 'effects #p"gray-filter.frag"))

(define-shader-pass box-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (box-blur-pass :fragment-shader)
  (pool-path 'effects #p"box-blur.frag"))

(define-shader-pass sobel-pass (simple-post-effect-pass)
  ())

(define-class-shader (sobel-pass :fragment-shader)
  (pool-path 'effects #p"sobel.frag"))

(define-shader-pass gaussian-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (gaussian-blur-pass :fragment-shader)
  (pool-path 'effects #p"gaussian.frag"))

(define-shader-pass radial-blur-pass (simple-post-effect-pass)
  ())

(define-class-shader (radial-blur-pass :fragment-shader)
  (pool-path 'effects #p"radial-blur.frag"))

(define-shader-pass fxaa-pass (simple-post-effect-pass)
  ())

(define-class-shader (fxaa-pass :fragment-shader)
  (pool-path 'effects #p"fxaa.frag"))

(define-shader-pass blend-pass (post-effect-pass)
  ((a-pass :port-type input)
   (b-pass :port-type input)
   (color :port-type output)))

(define-class-shader (blend-pass :fragment-shader)
  (pool-path 'effects #p"blend.frag"))

(define-shader-pass high-pass-filter (simple-post-effect-pass)
  ())

(define-class-shader (high-pass-filter :fragment-shader)
  (pool-path 'effects #p"high-pass-filter.frag"))

(define-shader-pass low-pass-filter (simple-post-effect-pass)
  ())

(define-class-shader (low-pass-filter :fragment-shader)
  (pool-path 'effects #p"low-pass-filter.frag"))

(define-shader-pass chromatic-aberration-filter (simple-post-effect-pass)
  ())

(define-class-shader (chromatic-aberration-filter :fragment-shader)
  (pool-path 'effects #p"aberration.frag"))

(define-shader-pass black-render-pass (render-pass)
  ((color :port-type output)))

(define-class-shader (black-render-pass :fragment-shader)
  "out vec4 color;

void main(){
  color *= vec4(0, 0, 0, 1);
}")

(define-shader-pass light-scatter-pass (post-effect-pass)
  ((previous-pass :port-type input)
   (black-render-pass :port-type input)
   (color :port-type output)))

(define-class-shader (light-scatter-pass :fragment-shader)
  (pool-path 'effects #p"light-scatter.frag"))

(define-shader-pass visualizer-pass (post-effect-pass)
  ((t[0] :port-type input)
   (t[1] :port-type input)
   (t[2] :port-type input)
   (t[3] :port-type input)
   (color :port-type output :texspec (:internal-format :rgba))))

(defmethod check-consistent ((pass visualizer-pass))
  ;; Skip consistency checks to allow optional inputs
  T)

(define-class-shader (visualizer-pass :fragment-shader)
  "out vec4 color;
in vec2 tex_coord;
uniform sampler2D t[4];
uniform int channel_count[4] = int[4](4,4,4,4);
uniform int textures_per_line = 1;

void main(){
  // Determine which texture we're currently in.
  int x = int(mod(tex_coord.x*textures_per_line, textures_per_line));
  int y = int(mod(tex_coord.y*textures_per_line, textures_per_line));
  int i = x+y*textures_per_line;

  // Compute texture and local UV
  vec2 uv = vec2(mod(tex_coord.x, textures_per_line)-float(x)/textures_per_line,
                 mod(tex_coord.y, textures_per_line)-float(y)/textures_per_line)
            *textures_per_line;

  // Sample the texture
  vec4 local = vec4(0);
  // Apparently we can't index with a dynamic var...
  switch(i){
  case 0: local = texture(t[0], uv); break;
  case 1: local = texture(t[1], uv); break;
  case 2: local = texture(t[2], uv); break;
  case 3: local = texture(t[3], uv); break;
  }

  int channels = channel_count[i];
  switch(channels){
  case 0: color = vec4(0); break;
  case 1: color = vec4(local.r, local.r, local.r, 1); break;
  case 2: color = vec4(local.rg, 0, 1); break;
  case 3: color = vec4(local.rgb, 1); break;
  case 4: color = local; break;
  }
}")
