#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defconstant MAX-LIGHTS 128)

(define-shader-pass geometry-pass (per-object-pass)
  ((depth    :port-type output
             :attachment :depth-stencil-attachment)
   (position :port-type output
             :attachment :color-attachment0
             :texspec (:internal-format :rgb16f
                       :pixel-type :float
                       :min-filter :linear
                       :mag-filter :linear
                       :wrapping :clamp-to-edge))
   (normal   :port-type output
             :attachment :color-attachment1
             :texspec (:internal-format :rgb16f
                       :pixel-type :float
                       :min-filter :linear
                       :mag-filter :linear))
   (albedo   :port-type output
             :attachment :color-attachment2
             :texspec (:internal-format :rgb
                       :min-filter :linear
                       :mag-filter :linear))
   (metal    :port-type output
             :attachment :color-attachment3
             :texspec (:internal-format :rgb
                       :min-filter :linear
                       :mag-filter :linear)))
  (:inhibit-shaders (shader-entity :fragment-shader)))

(define-class-shader (geometry-pass :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texcoord;
layout (location = 2) in vec3 normal;
layout (location = 3) in vec3 tangent;

out GEOM{
  vec3 position;
  vec2 texcoord;
  mat3 TBN;
} geom;

uniform mat4 model_matrix;

void main(){
  geom.position = vec3(model_matrix * vec4(position, 1.0));
  geom.texcoord = texcoord;

  vec3 T = normalize(mat3(model_matrix) * tangent);
  vec3 N = normalize(mat3(model_matrix) * normal);
  T = normalize(T - dot(T, N) * N);
  vec3 B = cross(N, T);
  geom.TBN = mat3(T, B, N);
}")

(define-class-shader (geometry-pass :fragment-shader)
  "
layout (location = 0) out vec3 position_map;
layout (location = 1) out vec3 normal_map;
layout (location = 2) out vec3 albedo_map;
layout (location = 3) out vec3 metal_map;

in GEOM{
  vec3 position;
  vec2 texcoord;
  mat3 TBN;
} geom;

uniform sampler2D diffuse;
uniform sampler2D specular;
uniform sampler2D normal;
uniform sampler2D roughness;
uniform sampler2D occlusion;

void main(){
    vec3 local_normal = texture(normal, geom.texcoord).rgb;
    local_normal = normalize(local_normal * 2.0 - 1.0);   
    local_normal = normalize(geom.TBN * local_normal);

    position_map = geom.position;
    normal_map = local_normal;
    albedo_map = texture(diffuse, geom.texcoord).rgb;
    metal_map.r = texture(specular, geom.texcoord).r;
    metal_map.g = texture(roughness, geom.texcoord).r;
    metal_map.b = texture(occlusion, geom.texcoord).r;
}")

(define-shader-entity geometry-shaded (vertex-entity)
  ((diffuse-map :initarg :diffuse-map :accessor diffuse-map)
   (specular-map :initarg :specular-map :accessor specular-map)
   (normal-map :initarg :normal-map :accessor normal-map)
   (roughness-map :initarg :roughness-map :accessor roughness-map)
   (occlusion-map :initarg :occlusion-map :accessor occlusion-map)))

(defmethod render :before ((entity geometry-shaded) (program shader-program))
  (setf (uniform program "diffuse") 0)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (gl-name (diffuse-map entity)))
  (setf (uniform program "specular") 1)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (gl-name (specular-map entity)))
  (setf (uniform program "normal") 2)
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d (gl-name (normal-map entity)))
  (setf (uniform program "roughness") 3)
  (gl:active-texture :texture3)
  (gl:bind-texture :texture-2d (gl-name (roughness-map entity)))
  (setf (uniform program "occlusion") 4)
  (gl:active-texture :texture4)
  (gl:bind-texture :texture-2d (gl-name (occlusion-map entity))))

(defmethod render :around ((pass geometry-pass) target)
  (with-pushed-attribs
    (disable :blend)
    (call-next-method)))

(define-shader-pass deferred-render-pass (post-effect-pass)
  ((position-map :port-type input)
   (normal-map :port-type input)
   (albedo-map :port-type input)
   (metal-map :port-type input)
   (color :port-type output :attachment :color-attachment0
          :texspec (:internal-format :rgba)))
  (:buffers (trial light-block)))

(defmethod render :before ((pass deferred-render-pass) (program shader-program))
  (setf (uniform program "view_position") (location (unit :camera *scene*))))

(define-gl-struct light
  (type :int)
  (position :vec3)
  (direction :vec3)
  (color :vec3)
  (attenuation-linear :float)
  (attenuation-quadratic :float)
  (outer :float)
  (cutoff :float))

(define-gl-struct light-block
  (lights (:array (:struct light) #.MAX-LIGHTS))
  (count :int))

(define-asset (trial light-block) static
    (make-instance 'uniform-buffer :struct-class 'light-block))

(define-class-shader (deferred-render-pass :fragment-shader)
  ;; KLUDGE
  ;; (gl-source (asset 'trial 'light-block))
  ;; KLUDGE
  (asdf:system-relative-pathname :trial "data/deferred-pbr.frag"))
