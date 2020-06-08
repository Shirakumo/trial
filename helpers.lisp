#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass located-entity (transformed entity)
  ((location :initarg :location :initform (vec 0 0 0) :accessor location)))

(defmethod apply-transforms progn ((obj located-entity))
  (translate (location obj)))

(defclass oriented-entity (transformed entity)
  ((orientation :initarg :orientation :initform (vec 1 0 0) :accessor orientation)
   (up :initarg :up :initform (vec 0 1 0) :accessor up)))

(defmethod apply-transforms progn ((obj oriented-entity))
  (rotate (vc (up obj) (orientation obj))
          (* 180 (/ (acos (v. (up obj) (orientation obj))) PI))))

(defclass rotated-entity (transformed entity)
  ((rotation :initarg :rotation :initform (vec 0 0 0) :accessor rotation)))

(defmethod apply-transforms progn ((obj rotated-entity))
  (rotate +vx+ (vx (rotation obj)))
  (rotate +vy+ (vy (rotation obj)))
  (rotate +vz+ (vz (rotation obj))))

(defclass axis-rotated-entity (transformed entity)
  ((axis :initarg :axis :initform (vec 0 1 0) :accessor axis)
   (angle :initarg :angle :initform 0 :accessor angle)))

(defmethod apply-transforms progn ((obj axis-rotated-entity))
  (rotate (axis obj) (angle obj)))

(defclass pivoted-entity (transformed entity)
  ((pivot :initarg :pivot :initform (vec 0 0 0) :accessor pivot)))

(defmethod apply-transforms progn ((obj pivoted-entity))
  (translate (pivot obj)))

(defclass scaled-entity (transformed entity)
  ((scaling :initarg :scaling :initform (vec 1 1 1) :accessor scaling)))

(defmethod apply-transforms progn ((obj scaled-entity))
  (scale (scaling obj)))

(defclass clocked-entity (clock listener)
  ())

(defmethod handle :before ((ev tick) (entity clocked-entity))
  (flare:update entity))

(define-shader-entity vertex-entity (renderable)
  ((vertex-array :initarg :vertex-array :accessor vertex-array)))

(defmethod stage :after ((entity vertex-entity) (area staging-area))
  (stage (vertex-array entity) area))

(defmethod render ((entity vertex-entity) (program shader-program))
  (setf (uniform program "model_matrix") (model-matrix))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (let ((vao (vertex-array entity)))
    (gl:bind-vertex-array (gl-name vao))
    ;; KLUDGE: Bad for performance!
    (if (find 'vertex-buffer (bindings vao) :key #'type-of)
        (%gl:draw-elements (vertex-form vao) (size vao) :unsigned-int 0)
        (%gl:draw-arrays (vertex-form vao) 0 (size vao)))
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

(defmethod render :before ((obj colored-entity) (program shader-program))
  (setf (uniform program "objectcolor") (color obj)))

(define-class-shader (colored-entity :fragment-shader)
  "uniform vec4 objectcolor;
out vec4 color;

void main(){
  color *= objectcolor;
}")

(define-shader-entity vertex-colored-entity ()
  ())

(define-class-shader (vertex-colored-entity :vertex-shader)
  "layout (location = 1) in vec4 in_vertexcolor;
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

(defmethod stage :after ((entity textured-entity) (area staging-area))
  (stage (texture entity) area))

(defmethod render :around ((obj textured-entity) (program shader-program))
  (let ((tex (texture obj)))
    (when tex
      (gl:active-texture :texture0)
      (gl:bind-texture (target tex) (gl-name tex))
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
