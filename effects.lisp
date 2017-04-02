#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-shader-pass negative-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader negative-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
  color = vec4(1.0-color.x, 1.0-color.y, 1.0-color.z, color.w);
}")

(define-shader-pass grayscale-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader grayscale-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
  float avg = 0.2126*color.x + 0.7152*color.y + 0.0722*color.z;
  color = vec4(avg, avg, avg, color.w);
}")

(define-shader-pass box-blur-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader box-blur-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  ivec2 size = textureSize(previousPass, 0);
  float blurSizeH = 1.0 / size.x;
  float blurSizeV = 1.0 / size.y;
  vec4 sum = vec4(0.0);
  for (int x=-4; x<=4; x++){
    for (int y=-4; y<=4; y++){
      sum += texture(previousPass,
                     vec2(texCoord.x + x*blurSizeH, texCoord.y + y*blurSizeV)) / 81.0;
    }
  }
  color = sum;
}")

(define-shader-pass sobel-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader sobel-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  ivec2 size = textureSize(previousPass, 0);
  float sobelSizeH = 1.0 / size.x;
  float sobelSizeV = 1.0 / size.y;
  vec4 top         = texture(previousPass, vec2(texCoord.x, texCoord.y + sobelSizeV));
  vec4 bottom      = texture(previousPass, vec2(texCoord.x, texCoord.y - sobelSizeV));
  vec4 left        = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y));
  vec4 right       = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y));
  vec4 topLeft     = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y + sobelSizeV));
  vec4 topRight    = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y + sobelSizeV));
  vec4 bottomLeft  = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y - sobelSizeV));
  vec4 bottomRight = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y - sobelSizeV));
  vec4 sx = -topLeft - 2 * left - bottomLeft + topRight   + 2 * right  + bottomRight;
  vec4 sy = -topLeft - 2 * top  - topRight   + bottomLeft + 2 * bottom + bottomRight;
  color = sqrt(sx * sx + sy * sy);
}")

(define-shader-pass gaussian-blur-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader gaussian-blur-pass :fragment-shader
  "
in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float blurRadius = 2;
uniform vec2 dir = vec2(1.0, 0.0);

void main() {
  ivec2 size = textureSize(previousPass, 0);
  // FIXME: Calculate the dimension properly
  float blur = blurRadius;
  if(dir.x == 1.0)
    blur /= size.x;
  else
    blur /= size.y;
  vec2 tc = texCoord;
  color = vec4(0.0);

  color += texture2D(previousPass, vec2(tc.x - 4.0*blur*dir.x, tc.y - 4.0*blur*dir.y)) * 0.0162162162;
  color += texture2D(previousPass, vec2(tc.x - 3.0*blur*dir.x, tc.y - 3.0*blur*dir.y)) * 0.0540540541;
  color += texture2D(previousPass, vec2(tc.x - 2.0*blur*dir.x, tc.y - 2.0*blur*dir.y)) * 0.1216216216;
  color += texture2D(previousPass, vec2(tc.x - 1.0*blur*dir.x, tc.y - 1.0*blur*dir.y)) * 0.1945945946;

  color += texture2D(previousPass, vec2(tc.x, tc.y)) * 0.2270270270;

  color += texture2D(previousPass, vec2(tc.x + 1.0*blur*dir.x, tc.y + 1.0*blur*dir.y)) * 0.1945945946;
  color += texture2D(previousPass, vec2(tc.x + 2.0*blur*dir.x, tc.y + 2.0*blur*dir.y)) * 0.1216216216;
  color += texture2D(previousPass, vec2(tc.x + 3.0*blur*dir.x, tc.y + 3.0*blur*dir.y)) * 0.0540540541;
  color += texture2D(previousPass, vec2(tc.x + 4.0*blur*dir.x, tc.y + 4.0*blur*dir.y)) * 0.0162162162;
}")
