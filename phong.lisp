#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity phong (vertex-entity)
  ((diffuse :initarg :diffuse :accessor diffuse)
   (specular :initarg :specular :accessor specular)
   (normal :initarg :normal :accessor normal)))

(defmethod render :before ((obj phong) (shader shader-program))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (gl-name (diffuse obj)))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (gl-name (specular obj)))
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d (gl-name (normal obj)))
  (setf (uniform shader "material.diffuse") 0)
  (setf (uniform shader "material.specular") 1)
  (setf (uniform shader "material.normal") 2)
  (setf (uniform shader "material.shininess") 0.1)
  (setf (uniform shader "light.color") (vec3 1 1 1))
  (setf (uniform shader "light.ambient") 0.3)
  (setf (uniform shader "light.position") (vec 40 0 20))
  (setf (uniform shader "light.direction") (vec -100 0 0))
  (setf (uniform shader "light.cutoff") (coerce (cos (deg->rad 60)) 'single-float))
  (setf (uniform shader "light.outer") (coerce (cos (deg->rad 70)) 'single-float))
  (setf (uniform shader "camera_pos") (location (unit :camera (scene +main+)))))

(define-class-shader (phong :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec2 in_texcoord;
layout (location = 2) in vec3 in_normal;

uniform vec3 camera_pos;
uniform mat4 model_matrix;

out vec2 texcoord;
out vec3 fragment_pos;
out vec3 normal;

void main(){
  fragment_pos = vec3(model_matrix * vec4(position, 1.0));
  texcoord = in_texcoord;

  normal = mat3(transpose(inverse(model_matrix))) * in_normal;
}")

(define-class-shader (phong :fragment-shader)
  "
struct Material {
  sampler2D diffuse;
  sampler2D specular;
  sampler2D normal;
  float shininess;
};

struct Light {
  vec3 position;
  vec3 direction;
  float cutoff;
  float outer;
  vec3 color;
  float ambient;
};

uniform vec3 camera_pos;
uniform Light light;
uniform Material material;

in vec2 texcoord;
in vec3 fragment_pos;
in vec3 normal;
out vec4 color;

mat3 cotangent_frame( vec3 N, vec3 p, vec2 uv ){
    // get edge vectors of the pixel triangle
    vec3 dp1 = dFdx( p );
    vec3 dp2 = dFdy( p );
    vec2 duv1 = dFdx( uv );
    vec2 duv2 = dFdy( uv );
 
    // solve the linear system
    vec3 dp2perp = cross( dp2, N );
    vec3 dp1perp = cross( N, dp1 );
    vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;
 
    // construct a scale-invariant frame 
    float invmax = inversesqrt( max( dot(T,T), dot(B,B) ) );
    return mat3( T * invmax, B * invmax, N );
}

vec3 shade_pointlight(Light light, Material material, vec3 normal){
  vec3 light_dir = normalize(light.position - fragment_pos);
  vec3 view_dir = normalize(camera_pos - fragment_pos);
  vec3 reflect_dir = reflect(-light_dir, normal);
  float distance = length(light.position - fragment_pos);
  float attenuation = 1.0 / (1.0 + 0.014 * distance + 0.0007 * distance * distance);

  vec3 ambient = vec3(texture(material.diffuse, texcoord)) * light.ambient;
  vec3 diffuse = vec3(texture(material.diffuse, texcoord)) * max(dot(normal, light_dir), 0);
  vec3 specular = vec3(texture(material.specular, texcoord)) * pow(max(dot(view_dir, reflect_dir), 0.0), 32);
  
  return light.color * attenuation * (ambient+diffuse+specular);
}

vec3 shade_spotlight(Light light, Material material, vec3 normal){
  vec3 light_dir = normalize(light.position - fragment_pos);
  vec3 view_dir = normalize(camera_pos - fragment_pos);
  vec3 reflect_dir = reflect(-light_dir, normal);
  float distance = length(light.position - fragment_pos);
  float attenuation = 1.0 / (1.0 + 0.0014 * distance + 0.000007 * distance * distance);
  float theta = dot(light_dir, normalize(-light.direction));

  vec3 ambient = vec3(texture(material.diffuse, texcoord)) * light.ambient;
  vec3 diffuse = vec3(0,0,0);
  vec3 specular = vec3(0,0,0);

  if(theta > light.outer) {
    float epsilon   = light.cutoff - light.outer;
    float intensity = clamp((theta - light.outer) / epsilon, 0.0, 1.0); 
    diffuse = intensity * vec3(texture(material.diffuse, texcoord)) * max(dot(normal, light_dir), 0);
    specular = intensity * vec3(texture(material.specular, texcoord)) * pow(max(dot(view_dir, reflect_dir), 0.0), 32);
  }

  return light.color * attenuation * (ambient+diffuse+specular);
}

void main(){
  vec3 n = texture(material.normal, texcoord).rgb;
  //n = n * 255./127. - 128./127.;
  //n.y = -n.y;
  n = cotangent_frame(normal, fragment_pos-camera_pos, texcoord) * n;
  n = normalize(n);

  color.rgb = vec3(0,0,0);
  color.rgb += shade_spotlight(light, material, n);
  color.rgb += shade_pointlight(light, material, n);
}")
