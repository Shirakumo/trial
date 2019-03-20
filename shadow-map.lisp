#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass shadow-map-pass (per-object-pass)
  ((shadow :port-type output
           :attachment :depth-attachment
           :texspec (:width 1024
                     :height 1024
                     :wrapping :clamp-to-border
                     :border-color #.(vec 1 1 1 1)))
   (projection-matrix :initarg :projection-matrix :accessor shadow-projection-matrix)
   (view-matrix :initarg :view-matrix :accessor shadow-view-matrix))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(define-class-shader (shadow-map-pass :fragment-shader)
  "#version 330 core
void main(){}")

(defmethod coerce-pass-shader ((pass shadow-map-pass) class (type (eql :fragment-shader)))
  (effective-shader type pass))

(defmethod paint-with ((pass shadow-map-pass) target)
  (with-pushed-matrix ((projection-matrix (shadow-projection-matrix pass))
                       (view-matrix (shadow-view-matrix pass)))
    (gl:cull-face :front)
    (call-next-method)
    (gl:cull-face :back)))

(define-shader-pass shadow-render-pass ()
  ((shadow-map :port-type input)
   (shadow-map-pass :initarg :shadow-map-pass
                    :initform (error "SHADOW-MAP-PASS required.")
                    :accessor shadow-map-pass)))

(defmethod paint-with :before ((pass shadow-render-pass) thing)
  (let ((program (shader-program-for-pass pass thing))
        (shadow (shadow-map-pass pass)))
    (setf (uniform program "light_space_matrix") (m* (shadow-projection-matrix shadow)
                                                     (shadow-view-matrix shadow)))))

(define-class-shader (shadow-render-pass :fragment-shader 10)
  "
#define SHADOW_SAMPLES 4
#define SHADOW_SAMPLE_SPREAD 0.001
uniform sampler2D shadow_map;
uniform mat4 light_space_matrix;

vec2 poisson_disk[16] = vec2[](
   vec2( -0.94201624, -0.39906216 ),
   vec2( 0.94558609, -0.76890725 ),
   vec2( -0.094184101, -0.92938870 ),
   vec2( 0.34495938, 0.29387760 ),
   vec2( -0.91588581, 0.45771432 ),
   vec2( -0.81544232, -0.87912464 ),
   vec2( -0.38277543, 0.27676845 ),
   vec2( 0.97484398, 0.75648379 ),
   vec2( 0.44323325, -0.97511554 ),
   vec2( 0.53742981, -0.47373420 ),
   vec2( -0.26496911, -0.41893023 ),
   vec2( 0.79197514, 0.19090188 ),
   vec2( -0.24188840, 0.99706507 ),
   vec2( -0.81409955, 0.91437590 ),
   vec2( 0.19984126, 0.78641367 ),
   vec2( 0.14383161, -0.14100790 )
);

float random(vec4 seed4){
  float dot_product = dot(seed4, vec4(12.9898,78.233,45.164,94.673));
  return fract(sin(dot_product) * 43758.5453);
}

float shadow_factor(vec3 position, float bias){
  vec4 light_space_position = light_space_matrix * vec4(position, 1);
  vec3 projected = light_space_position.xyz / light_space_position.w;
  projected = (projected+1)*0.5;
  if(projected.z > 1) return 0;
  float closest = texture(shadow_map, projected.xy).r;
  float current = projected.z;
  float shadow = 0;
  vec2 texel_size = 1.0 / textureSize(shadow_map, 0);
  for(int i=0; i<SHADOW_SAMPLES; ++i){
    int index = int(16*random(vec4(gl_FragCoord.xyy, i)))%16;
    vec2 poisson = poisson_disk[index]*SHADOW_SAMPLE_SPREAD;
    for(int x=-1; x<=1; ++x){
      for(int y=-1; y<=1; ++y){
        float closest = texture(shadow_map, projected.xy + poisson + vec2(x, y)*texel_size).r;
        if((current - bias) > closest)
          shadow+=(1.0 / (SHADOW_SAMPLES*8.0));
      }
    }
  }
  return shadow;
}

float shadow_bias(vec3 normal, vec3 light_direction){
  return clamp(0.005*tan(acos(dot(normal, light_direction))), 0, 0.001);
}")
