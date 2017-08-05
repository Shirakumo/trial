#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-subject skybox ()
  ((texture :initarg :texture :accessor texture)
   (vertex-array :initform NIL :Accessor vertex-array))
  (:default-initargs :texture (error "TEXTURE required.")))

(defmethod load progn ((skybox skybox))
  (load (texture skybox))
  (setf (vertex-array skybox) (gl:gen-vertex-array)))

(defmethod offload progn ((skybox skybox))
  (offload (texture skybox))
  (gl:delete-vertex-arrays (list (vertex-array skybox))))

(defmethod paint ((skybox skybox) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass skybox))
        (texture (texture skybox)))
    (setf (uniform shader "view_matrix") (view-matrix))
    (setf (uniform shader "projection_matrix") (projection-matrix))
    (gl:depth-mask NIL)
    (gl:bind-vertex-array (vertex-array skybox))
    (gl:active-texture :texture0)
    (gl:bind-texture (target texture) (resource texture))
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-texture (target texture) 0)
    (gl:bind-vertex-array 0)
    (gl:depth-mask T)))

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

    gl_Position = position;
}")

(define-class-shader (skybox :fragment-shader)
  "uniform samplerCube texture_image;
smooth in vec3 eye;

out vec4 color;

void main() {
    color = texture(texture_image, eye);
}")
