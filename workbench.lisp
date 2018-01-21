(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :TEAPOT01MESH)

(define-asset (workbench table) mesh
    ((make-cube '(500 500 2))))

(define-asset (workbench wood) texture
    (#p"wood.jpg"))

(define-asset (workbench lamp) mesh
    (#p"lamp.DAE")
  :mesh :SPHERE02MESH)

(define-asset (workbench cube) mesh
    ((make-cube 10)))

(define-asset (workbench white) texture
    (#p"white.png"))

(define-asset (workbench metal) texture
    (#p"metal.jpg"))

(define-shader-entity phong (vertex-entity)
  ((diffuse :initarg :diffuse :accessor diffuse)
   (specular :initarg :specular :accessor specular)))

(defmethod paint :before ((obj phong) (pass shader-pass))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (resource (diffuse obj)))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (resource (specular obj)))
  (let ((shader (shader-program-for-pass pass obj)))
    (setf (uniform shader "material.diffuse") 0)
    (setf (uniform shader "material.specular") 1)
    (setf (uniform shader "material.shininess") 0.5)
    (setf (uniform shader "light.color") (vec3 1 1 1))
    (setf (uniform shader "light.ambient") 0.1)
    (setf (uniform shader "light.position") (vec 0 80 0))
    (setf (uniform shader "light.direction") (vec -100 -80 0))
    (setf (uniform shader "light.cutoff") (coerce (cos (deg->rad 20)) 'single-float))
    (setf (uniform shader "light.outer") (coerce (cos (deg->rad 25)) 'single-float))
    (setf (uniform shader "camera_pos") (location (unit :camera (scene (window :main)))))))

(define-class-shader (phong :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec2 in_texcoord;
layout (location = 2) in vec3 in_normal;

uniform mat4 model_matrix;

out vec2 texcoord;
out vec3 normal;
out vec3 fragment_pos;

void main(){
  normal = mat3(transpose(inverse(model_matrix))) * in_normal;
  fragment_pos = vec3(model_matrix * vec4(position, 1.0));
  texcoord = in_texcoord;
}")

(define-class-shader (phong :fragment-shader)
  "
struct Material {
  sampler2D diffuse;
  sampler2D specular;
  float shininess;
}

struct Light {
  vec3 position;
  vec3 direction;
  float cutoff;
  float outer;
  vec3 color;
  float ambient;
}

uniform vec3 camera_pos;
uniform Light light;
uniform Material material;

in vec2 texcoord;
in vec3 normal;
in vec3 fragment_pos;
out vec4 color;

vec3 shade_pointlight(Light light, Material material){
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

vec3 shade_spotlight(Light light, Material material){
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
  color.rgb = vec3(0,0,0);
  color.rgb += shade_spotlight(light, material);
  color.rgb += shade_pointlight(light, material);
}")

(define-shader-subject teapot (phong located-entity rotated-entity)
  ((vel :initform 0.01 :accessor vel))
  (:default-initargs :vertex-array (asset 'workbench 'teapot)
                     :rotation (vec (/ PI -2) 0 0)
                     :location (vec -50 0 0)
                     :diffuse (asset 'workbench 'white)
                     :specular (asset 'workbench 'white)))

(define-shader-subject table (phong)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'table)
                     :diffuse (asset 'workbench 'wood)
                     :specular (asset 'workbench 'wood)))

(define-shader-subject lamp (phong located-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'lamp)
                     :location (vec 0 85 0)
                     :diffuse (asset 'workbench 'metal)
                     :specular (asset 'workbench 'metal)))

(define-handler (teapot tick) (ev)
  (incf (vz (rotation teapot)) (vel teapot)))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'table) scene)
    (enter (make-instance 'lamp) scene)
    (enter (make-instance 'teapot) scene)
    (enter (make-instance 'teapot :vertex-array (asset 'workbench 'cube) :location (vec -70 50 0)) scene)
    (enter (make-instance 'editor-camera :location (vec 70 100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
