(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity skybox (fullscreen-entity)
  ((texture :initarg :texture :accessor texture))
  (:default-initargs :texture (error "TEXTURE required.")))

(defmethod stage :after ((skybox skybox) (area staging-area))
  (stage (texture skybox) area))

(defmethod bind-textures ((skybox skybox))
  (gl:active-texture :texture0)
  (activate (texture skybox)))

(defmethod render ((skybox skybox) (shader shader-program))
  (setf (uniform shader "view_matrix") (view-matrix))
  (setf (uniform shader "projection_matrix") (projection-matrix))
  (setf (uniform shader "texture_image") 0)
  (gl:depth-mask NIL)
  (render-array (// 'trial 'fullscreen-square))
  (gl:depth-mask T))

(define-class-shader (skybox :vertex-shader)
  "uniform mat4 projection_matrix;
uniform mat4 view_matrix;

smooth out vec3 eye;

void main@after() {
    mat4 inverseProjection = inverse(projection_matrix);
    mat3 inverseModelview = transpose(mat3(view_matrix));
    vec3 unprojected = (inverseProjection * vec4(position,1)).xyz;
    eye = inverseModelview * unprojected;
    eye.y *= -1;
}")

(define-class-shader (skybox :fragment-shader)
  "uniform samplerCube texture_image;
smooth in vec3 eye;

out vec4 color;

void main() {
  color = texture(texture_image, eye);
}")

(define-shader-pass skybox-pass (single-shader-pass skybox)
  ((color :port-type output)))
