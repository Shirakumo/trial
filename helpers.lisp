#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass located-entity (entity)
  ((location :initarg :location :initform (vec 0 0 0) :accessor location)))

(defmethod paint :around ((obj located-entity) target)
  (with-pushed-matrix ()
    (translate (location obj))
    (call-next-method)))

(defclass oriented-entity (entity)
  ((orientation :initarg :orientation :initform (vec 1 0 0) :accessor orientation)
   (up :initarg :up :initform (vec 0 1 0) :accessor up)))

(defmethod paint :around ((obj oriented-entity) target)
  (with-pushed-matrix ()
    (rotate (vc (up obj) (orientation obj))
            (* 180 (/ (acos (v. (up obj) (orientation obj))) PI)))
    (call-next-method)))

(defclass rotated-entity (entity)
  ((rotation :initarg :rotation :initform (vec 0 0 0) :accessor rotation)))

(defmethod paint :around ((obj rotated-entity) target)
  (with-pushed-matrix ()
    (rotate +vx+ (vx (rotation obj)))
    (rotate +vy+ (vy (rotation obj)))
    (rotate +vz+ (vz (rotation obj)))
    (call-next-method)))

(defclass axis-rotated-entity (entity)
  ((axis :initarg :axis :initform (vec 0 1 0) :accessor axis)
   (angle :initarg :angle :initform 0 :accessor angle)))

(defmethod paint :around ((obj axis-rotated-entity) target)
  (with-pushed-matrix ()
    (rotate (axis obj) (angle obj))
    (call-next-method)))

(defclass pivoted-entity (entity)
  ((pivot :initarg :pivot :initform (vec 0 0 0) :accessor pivot)))

(defmethod paint :around ((obj pivoted-entity) target)
  (with-pushed-matrix ()
    (translate (pivot obj))
    (call-next-method)))

(define-subject clocked-subject (clock)
  ())

(define-handler (clocked-subject advance-time tick) (ev)
  (flare:update clocked-subject))

(define-shader-entity vertex-entity ()
  ((vertex-array :initarg :vertex-array :accessor vertex-array)
   (vertex-form :initarg :vertex-form :initform :triangles :accessor vertex-form)))

(defmethod paint ((subject vertex-entity) (pass shader-pass))
  (let ((program (shader-program-for-pass pass subject)))
    (setf (uniform program "model_matrix") (model-matrix))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix)))
  (let ((vao (vertex-array subject)))
    (gl:bind-vertex-array (resource vao))
    (%gl:draw-elements (vertex-form subject) (size vao) :unsigned-int 0)
    (gl:bind-vertex-array 0)))

(define-class-shader (vertex-entity :vertex-shader)
  "layout (location = 0) in vec3 position;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 1.0f);
}")

(define-shader-entity colored-entity ()
  ((color :initform (vec 0 0 1 1) :reader color)))

(defmethod shared-initialize :after ((entity colored-entity) slots &key color)
  (when color (setf (color entity) color)))

(defmethod (setf color) ((color vec3) (entity colored-entity))
  (setf (color entity) (vec4 (vx color) (vy color) (vz color) 1)))

(defmethod (setf color) ((color vec4) (entity colored-entity))
  (setf (slot-value entity 'color) color))

(defmethod paint :before ((obj colored-entity) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass obj)))
    (setf (uniform shader "objectcolor") (color obj))))

(define-class-shader (colored-entity :fragment-shader)
  "uniform vec4 objectcolor;
out vec4 color;

void main(){
  color *= objectcolor;
}")

(define-shader-entity vertex-colored-entity ()
  ())

(define-class-shader (vertex-colored-entity :vertex-shader)
  "layout (location = 2) in vec4 in_vertexcolor;
out vec4 vertexcolor;

void main(){
  vertexcolor = in_vertexcolor;
}")

(define-class-shader (vertex-colored-entity :fragment-shader)
  "in vec4 vertexcolor;
out vec4 color;

void main(){
  color *= vertexcolor;
}")

(define-shader-entity textured-entity ()
  ((texture :initform NIL :initarg :texture :accessor texture)))

(defmethod paint :around ((obj textured-entity) target)
  (let ((tex (texture obj)))
    (when tex
      (gl:active-texture :texture0)
      (gl:bind-texture (target tex) (resource tex))
      (call-next-method)
      (gl:bind-texture (target tex) 0))))

(define-class-shader (textured-entity :vertex-shader)
  "layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;

void main(){
  texcoord = in_texcoord;
}")

(define-class-shader (textured-entity :fragment-shader)
  "in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  color *= texture(texture_image, texcoord);
}")
