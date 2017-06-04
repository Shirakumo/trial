#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-subject clocked-subject (clock)
  ())

(define-handler (clocked-subject advance-time tick) (ev)
  (update clocked-subject))

(define-shader-subject vertex-subject ()
  ((vertex-array :initarg :vertex-array :accessor vertex-array)
   (vertex-form :initarg :vertex-form :initform :triangles :accessor vertex-form)))

(defmethod paint ((subject vertex-subject) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass subject)))
    (setf (uniform shader "model_matrix") (model-matrix))
    (setf (uniform shader "view_matrix") (view-matrix))
    (setf (uniform shader "projection_matrix") (projection-matrix)))
  (let ((vao (vertex-array subject)))
    (gl:bind-vertex-array (resource vao))
    (%gl:draw-elements (vertex-form subject) (size vao) :unsigned-int 0)
    (gl:bind-vertex-array 0)))

(defmethod load progn ((subject vertex-subject))
  (load (vertex-array subject)))

(define-class-shader vertex-subject :vertex-shader
  "layout (location = 0) in vec3 position;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
}")

;; FIXME: Make sure to coerce for proper colour format!
(define-shader-subject colored-subject ()
  ((color :initarg :color :initform (vec 0 0 1 1) :accessor color)))

(define-saved-initargs colored-subject color)

(defmethod paint :before ((obj colored-subject) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass obj)))
    (setf (uniform shader "objectcolor") (color obj))))

(define-class-shader colored-subject :fragment-shader
  "uniform vec4 objectcolor;
out vec4 color;

void main(){
  color *= objectcolor;
}")

(define-shader-subject vertex-colored-subject ()
  ())

(define-class-shader vertex-colored-subject :vertex-shader
  "layout (location = 2) in vec4 in_vertexcolor;
out vec4 vertexcolor;

void main(){
  vertexcolor = in_vertexcolor;
}")

(define-class-shader vertex-colored-subject :fragment-shader
  "in vec4 vertexcolor;
out vec4 color;

void main(){
  color *= vertexcolor;
}")

(define-shader-subject textured-subject ()
  ((texture :initform NIL :initarg :texture :accessor texture)))

(defmethod paint :around ((obj textured-subject) target)
  (let ((tex (texture obj)))
    (when tex
      (gl:active-texture :texture0)
      (gl:bind-texture (target tex) (resource tex))
      (call-next-method)
      (gl:bind-texture (target tex) 0))))

(defmethod load progn ((subject textured-subject))
  (load (texture subject)))

(define-class-shader textured-subject :vertex-shader
  "layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;

void main(){
  texcoord = in_texcoord;
}")

(define-class-shader textured-subject :fragment-shader
  "in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  color *= texture(texture_image, texcoord);
}")
