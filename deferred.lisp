#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass geometry-pass (per-object-pass)
  ((depth    :port-type output
             :attachment :depth-stencil-attachment)
   (position :port-type output
             :attachment :color-attachment0
             :texspec (:internal-format :rgb16f
                       :pixel-type :float))
   (normal   :port-type output
             :attachment :color-attachment1
             :texspec (:internal-format :rgb16f
                       :pixel-type :float))
   (albedo   :port-type output
             :attachment :color-attachment2
             :texspec (:internal-format :rgba)))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(define-class-shader (geometry-pass :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texcoord;
layout (location = 2) in vec3 normal;

out GEOM{
  vec3 position;
  vec2 texcoord;
  vec3 normal;
} geom;

uniform mat4 model_matrix;

void main(){
  geom.position = vec3(model_matrix * vec4(position, 1.0));
  geom.texcoord = texcoord;
  geom.normal = mat3(transpose(inverse(model_matrix))) * normal;
}")

(define-class-shader (geometry-pass :fragment-shader)
  "#version 330 core
layout (location = 0) out vec3 position_map;
layout (location = 1) out vec3 normal_map;
layout (location = 2) out vec4 albedo_map;

in GEOM{
  vec3 position;
  vec2 texcoord;
  vec3 normal;
} geom;

uniform sampler2D diffuse;
uniform sampler2D specular;

void main(){
    position_map = geom.position;
    normal_map = normalize(geom.normal);
    albedo_map.rgb = texture(diffuse, geom.texcoord).rgb;
    albedo_map.a = texture(specular, geom.texcoord).r;
}")

(define-shader-entity geometry-shaded (vertex-entity)
  ((diffuse-map :initarg :diffuse-map :accessor diffuse-map)
   (specular-map :initarg :specular-map :accessor specular-map)))

(defmethod paint :before ((entity geometry-shaded) (pass geometry-pass))
  (let ((program (shader-program-for-pass pass entity)))
    (setf (uniform program "diffuse") 0)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (gl-name (diffuse-map entity)))
    (setf (uniform program "specular") 1)
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (gl-name (specular-map entity)))))

(defmethod paint :around ((entity geometry-shaded) (pass geometry-pass))
  (with-pushed-attribs
    (disable :blend)
    (call-next-method)))

(define-shader-pass deferred-render-pass (post-effect-pass)
  ((position-map :port-type input)
   (normal-map :port-type input)
   (albedo-map :port-type input)
   (color :port-type output :attachment :color-attachment0))
  (:buffers (trial light-block)))

(defmethod paint-with :before ((pass deferred-render-pass) target)
  (let ((program (shader-program pass)))
    (setf (uniform program "view_position") (location (unit :camera *scene*)))))

(define-gl-struct light
  (type :int)
  (position :vec3)
  (direction :vec3)
  (color :vec3))

(define-gl-struct light-block
  (lights (:struct light) :array-size 32)
  (count :int))

(define-asset (trial light-block) uniform-buffer
    'light-block)

(define-class-shader (deferred-render-pass :fragment-shader)
  (gl-source (asset 'trial 'light-block))
  "out vec4 color;
in vec2 tex_coord;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D albedo_map;
uniform vec3 view_position;
float lighting_strength = 1.0;
const float PI = 3.14159265;

vec3 directional_light(Light light, vec3 view_direction, vec3 position, vec3 normal, vec4 albedo){
  vec3 light_direction = normalize(-light.direction);
  vec3 halfway_direction = normalize(light_direction+view_direction);
  vec3 diffuse = max(dot(normal, light_direction), 0.0) * albedo.rgb;
  float energy = (8.0 + 32) / (8.0 * PI);
  float specular = pow(max(dot(normal, halfway_direction), 0.0), 32) * albedo.a * energy;
  return (diffuse + specular) * light.color;
}

vec3 point_light(Light light, vec3 view_direction, vec3 position, vec3 normal, vec4 albedo){
  vec3 light_direction = normalize(light.position - position);
  vec3 halfway_direction = normalize(light_direction+view_direction);
  vec3 diffuse = max(dot(normal, light_direction), 0.0) * albedo.rgb;
  float energy = (8.0 + 32) / (8.0 * PI);
  float specular = pow(max(dot(normal, halfway_direction), 0.0), 32) * albedo.a * energy;
  float distance = length(light.position - position);
  float attenuation = 1.0 / (1.0 + 0.07 * distance + 0.017 * distance * distance);
  return (diffuse + specular) * light.color * attenuation;
}

void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec4 albedo = texture(albedo_map, tex_coord).rgba;
  
  vec3 lighting = vec3(0);
  vec3 view_direction = normalize(position - view_position);
  for(int i=0; i<light_block.count; ++i){
    Light light = light_block.lights[i];
    switch(light.type){
    case 0: break;
    case 1: lighting += directional_light(light, view_direction, position, normal, albedo); break;
    case 2: lighting += point_light(light, view_direction, position, normal, albedo); break;
    }
  }
  
  color = vec4(lighting*lighting_strength + albedo.rgb*0.1, 1.0);
}")
