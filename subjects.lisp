#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject clocked-subject (clock)
  ())

(define-handler (clocked-subject advance-time tick) (ev)
  (update clocked-subject))

(define-shader-subject vertex-subject ()
  ((vertex-array :initarg :vertex-array :accessor vertex-array)
   (vertex-form :initarg :vertex-form :initform :triangles :accessor vertex-form)))

(defmethod paint :before ((obj vertex-subject) target)
  (setf (uniform obj "model_matrix") (model-matrix))
  (setf (uniform obj "view_matrix") (view-matrix))
  (setf (uniform obj "projection_matrix") (projection-matrix)))

(defmethod paint ((subject vertex-subject) target)
  (let ((vao (vertex-array subject)))
    (gl:bind-vertex-array (resource vao))
    (%gl:draw-elements (vertex-form subject) (size vao) :unsigned-int 0)
    (gl:bind-vertex-array 0)))

(define-class-shader vertex-subject :vertex-shader
  "layout (location = 0) in vec3 position;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
}")

(define-shader-subject colored-subject ()
  ((color :initarg :color :initform (vec 0 0 1 1) :accessor color)))

(define-saved-initargs colored-subject color)

(defmethod paint :before ((obj colored-subject) target)
  (setf (uniform obj "objectcolor") (color obj)))

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
  ((texture :initform NIL :accessor texture)))

(defmethod paint :around ((obj textured-subject) target)
  (let* ((tex (texture obj))
         (target (target tex)))
    (when tex
      (gl:bind-texture target (resource tex))
      (call-next-method)
      (gl:bind-texture target 0))))

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
