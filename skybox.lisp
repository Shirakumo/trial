#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity skybox (renderable)
  ((texture :initarg :texture :accessor texture)
   (vertex-array :initform (// 'trial 'empty-vertex-array) :Accessor vertex-array))
  (:default-initargs :texture (error "TEXTURE required.")))

(defmethod stage :after ((skybox skybox) (area staging-area))
  (stage (texture skybox) area)
  (stage (vertex-array skybox) area))

(defmethod render ((skybox skybox) (shader shader-program))
  (setf (uniform shader "view_matrix") (view-matrix))
  (setf (uniform shader "projection_matrix") (projection-matrix))
  (with-render-settings (:no-depth-writes)
    (bind (texture skybox) NIL)
    (render (vertex-array skybox) NIL)))

(define-class-shader (skybox :vertex-shader)
  "const vec2 quad_vertices[4] = vec2[4](vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0), vec2(1.0, 1.0));

uniform mat4 projection_matrix;
uniform mat4 view_matrix;

smooth out vec3 eye;

void main() {
    vec4 position = vec4(quad_vertices[gl_VertexID], 0.0, 1.0);
    mat4 inverseProjection = inverse(projection_matrix);
    mat3 inverseModelview = transpose(mat3(view_matrix));
    vec3 unprojected = (inverseProjection * position).xyz;
    eye = inverseModelview * unprojected;
    eye.y *= -1;

    gl_Position = position;
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
