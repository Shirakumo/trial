#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass located-entity (transformed entity)
  ((location :initarg :location :initform (vec 0 0 0) :reader location)))

(defmethod (setf location) ((vec vec3) (obj located-entity))
  (v<- (location obj) vec))

(defmethod apply-transforms progn ((obj located-entity))
  (translate (location obj)))

(defclass sized-entity (entity)
  ((bsize :initarg :bsize :initform (vec 0 0 0) :accessor bsize)))

(defclass oriented-entity (transformed entity)
  ((orientation :initarg :orientation :initform (vec 1 0 0) :accessor orientation)
   (up :initarg :up :initform (vec 0 1 0) :accessor up)))

(defmethod apply-transforms progn ((obj oriented-entity))
  (rotate (vc (up obj) (orientation obj))
          (* 180 (/ (acos (v. (up obj) (orientation obj))) PI))))

(defclass rotated-entity (transformed entity)
  ((rotation :initarg :rotation :initform (quat) :accessor rotation)))

(defmethod apply-transforms progn ((obj rotated-entity))
  (let ((mat (mat4)))
    (declare (dynamic-extent mat))
    (nm* (model-matrix) (qmat4 (rotation obj) mat))))

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

(defclass transformed-entity (transformed entity)
  ((transform :initarg :transform :initform (transform) :accessor tf)))

(defmethod initialize-instance :after ((entity transformed-entity) &key location scaling orientation)
  (when location (setf (location entity) location))
  (when scaling (setf (scaling entity) scaling))
  (when orientation (setf (orientation entity) orientation)))

(defmethod apply-transforms progn ((obj transformed-entity))
  (let* ((marr (make-array 16 :element-type 'single-float))
         (mat (3d-matrices::%mat4 marr)))
    (declare (dynamic-extent mat marr))
    (nm* (model-matrix) (tmat4 (tf obj) mat))))

(defmethod location ((obj transformed-entity))
  (tlocation (tf obj)))

(defmethod (setf location) (vec (obj transformed-entity))
  (v<- (tlocation (tf obj)) vec))

(defmethod scaling ((obj transformed-entity))
  (tscaling (tf obj)))

(defmethod (setf scaling) (vec (obj transformed-entity))
  (v<- (tscaling (tf obj)) vec))

(defmethod orientation ((obj transformed-entity))
  (trotation (tf obj)))

(defmethod (setf orientation) (quat (obj transformed-entity))
  (q<- (trotation (tf obj)) quat))

(defmethod axis ((obj transformed-entity))
  (qaxis (trotation (tf obj))))

(defmethod (setf axis) (axis (obj transformed-entity))
  (setf (trotation (tf obj)) (qfrom-angle axis (angle obj))))

(defmethod angle ((obj transformed-entity))
  (qangle (trotation (tf obj))))

(defmethod (setf angle) (angle (obj transformed-entity))
  (setf (trotation (tf obj)) (qfrom-angle (axis obj) angle)))

(define-shader-entity fullscreen-entity (renderable)
  ((vertex-array :initform (// 'trial 'fullscreen-square) :accessor vertex-array)))

(defmethod stage :after ((entity fullscreen-entity) (area staging-area))
  (stage (vertex-array entity) area))

(defmethod render ((entity fullscreen-entity) (program shader-program))
  (declare (optimize speed))
  (let* ((vao (vertex-array entity))
         (size (size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (gl-name vao))
    (if (indexed-p vao)
        (%gl:draw-elements (vertex-form vao) size :unsigned-int 0)
        (%gl:draw-arrays (vertex-form vao) 0 size))
    (gl:bind-vertex-array 0)))

(define-class-shader (fullscreen-entity :vertex-shader)
  "layout (location = 0) in vec3 position;

void main(){
  gl_Position = vec4(position, 1.0f);
}")

(define-shader-entity vertex-entity (renderable)
  ((vertex-array :initarg :vertex-array :accessor vertex-array)))

(defmethod stage :after ((entity vertex-entity) (area staging-area))
  (when (slot-boundp entity 'vertex-array)
    (stage (vertex-array entity) area)))

(defmethod render ((entity vertex-entity) (program shader-program))
  (declare (optimize speed))
  (setf (uniform program "model_matrix") (model-matrix))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (let* ((vao (vertex-array entity))
         (size (size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (gl-name vao))
    ;; KLUDGE: Bad for performance!
    (if (loop for binding in (bindings vao)
              thereis (typep binding 'vertex-buffer))
        (%gl:draw-elements (vertex-form vao) size :unsigned-int 0)
        (%gl:draw-arrays (vertex-form vao) 0 size))
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

(define-shader-entity textured-entity (renderable)
  ((texture :initform NIL :initarg :texture :accessor texture)))

(defmethod stage :after ((entity textured-entity) (area staging-area))
  (stage (texture entity) area))

(defmethod bind-textures :after ((obj textured-entity))
  (%gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (gl-name (texture obj))))

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
