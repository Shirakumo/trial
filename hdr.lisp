#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass hdr-output-pass ()
  ((color :port-type output :texspec (:internal-format :rgba16f))))

(define-shader-pass high-color-pass ()
  ((high-pass :port-type output :texspec (:internal-format :rgba16f)
              :attachment :color-attachment1)))

(define-class-shader (high-color-pass :fragment-shader -100)
  "out vec4 color;
layout (location = 1) out vec4 high_pass;

void main(){
  float brightness = dot(color.rgb, vec3(0.2126, 0.7152, 0.0722));
  if(brightness > 0.5)
    high_pass = vec4(color.rgb, 1.0);
  else
    high_pass = vec4(0.0, 0.0, 0.0, 1.0);
}")

(define-shader-pass bloom-pass (tone-mapping-pass)
  ((previous-pass :port-type input :texspec (:internal-format :rgba16f))
   (high-pass :port-type input))
  (:inhibit-shaders (tone-mapping-pass :fragment-shader)))

;; KLUDGE: We can't change the previous_pass texture value so there's no way for us
;;         to influence the value that the tone-mapping-pass would read in order to
;;         factor in the bloom. This we replicate its behaviour here. Not great.
;;         use-relations in GLSL-toolkit would allow us to fix this.
(define-class-shader (bloom-pass :fragment-shader)
  "uniform sampler2D previous_pass;
uniform sampler2D high_pass;
in vec2 uv;
out vec4 color;
uniform float gamma = 2.2;
uniform float exposure = 0.75;

void main(){
  vec4 source = texture(previous_pass, uv);
  vec4 bloom = texture(high_pass, uv);
  vec3 hdr = source.rgb + bloom.rgb;
  vec3 mapped = vec3(1.0) - exp((-hdr) * exposure);
  mapped = pow(mapped, vec3(1.0 / gamma));
  color = vec4(mapped, source.a);
}")
