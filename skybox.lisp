(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity skybox (fullscreen-entity)
  ((texture :initarg :texture :accessor texture)
   (color :initarg :color :initform (vec3 1) :accessor color))
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
  (setf (uniform shader "multiplier") (color skybox))
  (with-depth-mask NIL
    (render-array (// 'trial 'fullscreen-square))))

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
    gl_Position.z = +1;
}")

(define-class-shader (skybox :fragment-shader)
  "uniform samplerCube texture_image;
uniform vec3 multiplier;
smooth in vec3 eye;

out vec4 color;

void main() {
  color = vec4(texture(texture_image, eye).rgb * multiplier, 1);
}")

(define-shader-pass skybox-pass (single-shader-pass skybox)
  ((color :port-type output)))
