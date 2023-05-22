#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defconstant MAX-LIGHTS 128)
(defconstant MAX-MATERIALS 64)

(define-gl-struct standard-light
  (type :int)
  (position :vec3)
  (direction :vec3)
  (color :vec3)
  (attenuation-linear :float)
  (attenuation-quadratic :float)
  (outer-radius :float)
  (cutoff-radius :float))

(define-gl-struct standard-light-block
  (lights (:array (:struct standard-light) #.MAX-LIGHTS))
  (light-count :int))

(define-asset (trial standard-light-block) uniform-block
    'standard-light-block
  :binding NIL)

(define-gl-struct standard-textures
  (albedo :int)
  (normal :int)
  (emission :int)
  (metal-rough-occlusion :int))

(define-gl-struct standard-material
  (textures (:array :int 8))
  (albedo-factor :vec4)
  (emission-factor :vec4)
  (metallic-factor :float)
  (roughness-factor :float)
  (occlusion-factor :float)
  (alpha-cutoff :float))

(define-gl-struct standard-material-block
  (materials (:array (:struct standard-material) #.MAX-MATERIALS))
  (material-count :int))

(define-asset (trial standard-material-block) uniform-block
    'standard-material-block
  :binding NIL)

(define-gl-struct standard-environment-information
  (view-matrix :mat4)
  (projection-matrix :mat4)
  (view-size :vec2)
  (camera-position :vec3)
  (tt :float)
  (dt :float)
  (fdt :float))

(define-asset (trial standard-environment-information) uniform-block
    'standard-environment-information
  :binding NIL)

(define-shader-pass standard-render-pass (per-object-pass)
  ((color :port-type output :texspec (:internal-format :rgba32f) :attachment :color-attachment0 :reader color)
   (normal :port-type output :texspec (:internal-format :rgb16f) :attachment :color-attachment1 :reader normal)
   (depth :port-type output :attachment :depth-stencil-attachment :reader depth)
   (tt :initform 0.0 :accessor tt)
   (dt :initform 0.0 :accessor dt)
   (frame-start :initform 0d0 :accessor frame-start))
  (:buffers (trial standard-environment-information)))

(define-class-shader (standard-render-pass :vertex-shader)
  (gl-source (asset 'trial 'standard-environment-information))
  "out vec3 v_position;
out vec3 v_normal;
out vec2 v_uv;")

(define-class-shader (standard-render-pass :fragment-shader)
  (gl-source (asset 'trial 'standard-light-block))
  (gl-source (asset 'trial 'standard-material-block))
  (gl-source (asset 'trial 'standard-environment-information))
  "in vec3 v_position;
in vec3 v_normal;
in vec2 v_uv;
layout (location = 0) out vec4 f_color;
layout (location = 1) out vec3 f_normal;")

(define-handler (standard-render-pass tick) (dt)
  (let ((dt (float dt 0f0)))
    (setf (dt standard-render-pass) dt)
    (setf (tt standard-render-pass) (+ (tt standard-render-pass) dt))))

(defmethod render :before ((pass standard-render-pass) target)
  (let* ((frame-time (current-time))
         (old-time (shiftf (frame-start pass) frame-time))
         (fdt (- old-time frame-time)))
    (with-buffer-tx (buffer (// 'trial 'standard-environment-information))
      (setf (slot-value buffer 'view-matrix) (view-matrix))
      (setf (slot-value buffer 'projection-matrix) (projection-matrix))
      (setf (slot-value buffer 'view-size) (vec2 (width (framebuffer pass)) (height (framebuffer pass))))
      (setf (slot-value buffer 'camera-position) (location (camera pass)))
      (setf (slot-value buffer 'tt) (tt pass))
      (setf (slot-value buffer 'dt) (dt pass))
      (setf (slot-value buffer 'fdt) (float fdt 0f0)))))

(define-shader-entity standard-renderable (renderable transformed-entity)
  ((material :initarg :material :accessor material)))

(define-class-shader (standard-renderable :vertex-shader)
  "layout (location = 0) in vec3 in_position;
layout (location = 1) in vec2 in_uv;
layout (location = 2) in vec3 in_normal;

void main(){
  vec4 position = projection_matrix * view_matrix * vec4(in_position, 1);
  gl_Position = position;
  v_position = position.xyz;
  v_uv = in_uv;
  v_normal = in_normal;
}")

(define-class-shader (standard-renderable :fragment-shader)
  "uniform int material_id;
uniform sampler2D[32] textures;

vec2 uv;
vec3 normal;
vec4 color;
StandardMaterial material;
void standard_init();
vec4 standard_shade(in StandardLight light);
vec4 standard_mix(in vec4 upper, in vec4 lower);
void standard_finish();

#define NONE -1;
#define ALBEDO 0;
#define NORMAL 1;
#define EMISSION 2;
#define METAL_ROUGH_OCCLUSION 3;
#define METALLIC 4;
#define ROUGHNESS 5;
#define OCCLUSION 6;

vec4 sample_texture(int id, vec2 uv){
  switch id{
  case NONE: return vec4(0);
  case ALBEDO:
  case NORMAL:
  case METAL_ROUGH_OCCLUUSION:
  case EMISSION: return texture(textures[material.textures[id]], uv);
  case METALLIC: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).xxxx;
  case ROUGHNESS: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).yyyy;
  case OCCLUSION: return texture(textures[material.textures[METAL_ROUGH_OCCLUSION]], uv).zzzz;
  default: return vec4(1,0,0,1);
  }
}

void main(){
  uv = v_uv;
  normal = v_normal;
  color = vec4(0);
  material = materials[material_id];

  standard_init(material, color);
  for(int light_idx = 0; light_idx<light_Count; ++light_idx){
    StandardLight light = lights[light_idx];
    vec4 local_color;
    color = standard_mix(material, standard_shade(material, light, local_color), color);
  }
  standard_finish(material);
}")
