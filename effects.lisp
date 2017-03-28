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
  color = vec4(1.0-color.x, 1.0-color.y, 1.0-color.z, 1.0);
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
  float avg = 0.2126*color.r + 0.7152*color.g + 0.0722*color.b;
  color = vec4(avg, avg, avg, 1.0);
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
                     vec2(texCoord.x + x * blurSizeH, texCoord.y + y * blurSizeV)) / 81.0;
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
