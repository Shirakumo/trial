(in-package #:org.shirakumo.fraf.trial)

(defclass transformed () ())
(defclass renderable () ())
(defclass dynamic-renderable (renderable) ())

(defclass located-entity (transformed entity)
  ((location :initarg :location :initform (vec 0 0 0) :reader location :reader 3ds:location)))

(defmethod (setf location) ((vec vec3) (obj located-entity))
  (v<- (location obj) vec))

(defmethod apply-transforms progn ((obj located-entity))
  (translate (location obj)))

(defclass sized-entity (entity)
  ((bsize :initarg :bsize :initform (vec 0 0 0) :accessor bsize :reader 3ds:bsize)))

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
         (mat (mat4 marr)))
    (declare (dynamic-extent mat marr))
    (nm* (model-matrix) (tmat (tf obj) mat))))

(defmethod location ((obj transformed-entity))
  (tlocation (tf obj)))

(defmethod 3ds:location ((obj transformed-entity))
  (tlocation (tf obj)))

(defmethod (setf location) (vec (obj transformed-entity))
  (v<- (tlocation (tf obj)) vec))

(defmethod scaling ((obj transformed-entity))
  (tscaling (tf obj)))

(defmethod (setf scaling) (vec (obj transformed-entity))
  (v<- (tscaling (tf obj)) vec))

(defmethod (setf scaling) ((value real) (obj transformed-entity))
  (vsetf (tscaling (tf obj)) value value value))

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
  (render (vertex-array entity) program))

(define-class-shader (fullscreen-entity :vertex-shader)
  "layout (location = 0) in vec3 position;

void main(){
  maybe_call_next_method();
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
  (render (vertex-array entity) program))

(define-class-shader (vertex-entity :vertex-shader)
  "layout (location = 0) in vec3 position;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  maybe_call_next_method();
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
  maybe_call_next_method();
  color *= objectcolor;
}")

(define-shader-entity vertex-colored-entity ()
  ())

(define-class-shader (vertex-colored-entity :vertex-shader)
  "layout (location = 4) in vec4 in_vertexcolor;
out vec4 vertexcolor;

void main(){
  maybe_call_next_method();
  vertexcolor = in_vertexcolor;
}")

(define-class-shader (vertex-colored-entity :fragment-shader)
  "in vec4 vertexcolor;
out vec4 color;

void main(){
  maybe_call_next_method();
  color *= vertexcolor;
}")

(define-shader-entity textured-entity (renderable)
  ((texture :initform NIL :initarg :texture :accessor texture)))

(defmethod stage :after ((entity textured-entity) (area staging-area))
  (stage (texture entity) area))

(defmethod bind-textures :after ((obj textured-entity))
  (%gl:active-texture :texture0)
  (gl:bind-texture (target (texture obj)) (gl-name (texture obj))))

(define-class-shader (textured-entity :vertex-shader)
  "layout (location = 2) in vec2 in_uv;
out vec2 uv;

void main(){
  maybe_call_next_method();
  uv = in_uv;
}")

(define-class-shader (textured-entity :fragment-shader)
  "in vec2 uv;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  maybe_call_next_method();
  color *= texture(texture_image, uv);
}")

(defmethod enter (thing (container 3ds:container))
  (3ds:enter thing container))

(defmethod leave (thing (container 3ds:container))
  (3ds:leave thing container))

(defmethod clear ((container 3ds:container))
  (3ds:clear container))

(defmethod location ((region 3ds:region))
  (3ds:location region))

(defmethod bsize ((region 3ds:region))
  (3ds:bsize region))

(defclass mesh-entity (entity)
  ((mesh-asset :initform NIL :initarg :asset :accessor mesh-asset)
   (mesh :initarg :mesh :initform NIL :accessor mesh)))

(defmethod stage :after ((entity mesh-entity) (op load-op))
  (register-load-observer op entity (mesh-asset entity))
  (stage (mesh-asset entity) op))

(defmethod observe-load-state ((entity mesh-entity) (asset asset) (state (eql :loaded)) (op load-op))
  (setf (mesh-asset entity) asset)
  (restage entity op))

(defmethod (setf mesh-asset) :after ((asset asset) (entity mesh-entity))
  (setf (mesh entity) (or (mesh entity) T)))

(defmethod (setf mesh) :after ((mesh mesh-data) (entity mesh-entity))
  (setf (vertex-array entity) (resource (mesh-asset entity) (name mesh))))

(defmethod (setf mesh) ((name string) (entity mesh-entity))
  (let ((mesh (gethash name (meshes (mesh-asset entity)))))
    (if mesh
        (setf (mesh entity) mesh)
        #-trial-release
        (error "No mesh named ~s found. The following are known:~{~%  ~s~}"
               name (alexandria:hash-table-keys (meshes (mesh-asset entity)))))))

(defmethod (setf mesh) ((anything (eql T)) (entity mesh-entity))
  (loop for mesh being the hash-values of (meshes (mesh-asset entity))
        do (return (setf (mesh entity) mesh))))

(defmethod material ((entity mesh-entity))
  (when (typep (mesh entity) 'mesh-data)
    (material (mesh entity))))

(defclass multi-mesh-entity (entity)
  ((mesh-asset :initform NIL :initarg :asset :accessor mesh-asset)
   (mesh :initarg :mesh :initform NIL :accessor mesh)))

(defmethod stage :after ((entity multi-mesh-entity) (op load-op))
  (register-load-observer op entity (mesh-asset entity))
  (stage (mesh-asset entity) op))

(defmethod observe-load-state ((entity multi-mesh-entity) (asset asset) (state (eql :loaded)) (op load-op))
  (setf (mesh-asset entity) asset)
  (restage entity op))

(defmethod (setf mesh-asset) :after ((asset asset) (entity multi-mesh-entity))
  (setf (mesh entity) (or (mesh entity) T)))

(defmethod (setf mesh) :after ((meshes cons) (entity multi-mesh-entity))
  (let ((arrays (make-array (length meshes))))
    (map-into arrays (lambda (mesh) (resource (mesh-asset entity) (name mesh))) meshes)
    (setf (vertex-arrays entity) arrays)))

(defmethod (setf mesh) ((name string) (entity multi-mesh-entity))
  (let ((mesh (gethash name (meshes (mesh-asset entity)))))
    (setf (mesh entity) (if mesh
                            (list mesh)
                            (or (loop for i from 0
                                      for mesh = (gethash (cons name i) (meshes (mesh-asset entity)))
                                      while mesh collect mesh)
                                #-trial-release
                                (error "No mesh named ~s found. The following are known:~{~%  ~s~}"
                                       name (alexandria:hash-table-keys (meshes (mesh-asset entity)))))))))

(defmethod (setf mesh) ((all (eql T)) (entity multi-mesh-entity))
  (setf (mesh entity) (alexandria:hash-table-values (meshes (mesh-asset entity)))))

(defclass lod-entity (entity)
  ((lods :initform #() :reader lods)))

(defmethod shared-initialize :after ((entity lod-entity) slots &key (lods NIL lods-p))
  (when lods-p (setf (lods entity) lods)))

(defgeneric select-lod (lod entity))

(defmethod select-lod ((default (eql T)) (entity lod-entity))
  (select-lod (camera (scene +main+)) entity))

(defclass lod ()
  ((mesh :initform () :initarg :mesh :accessor mesh)
   (threshold :initform 0.0 :initarg :threshold :accessor threshold)))

(defmethod (setf lods) ((lods vector) (entity lod-entity))
  (setf (slot-value entity 'lods) (sort lods #'< :key #'threshold)))

(defmethod (setf lods) ((lods sequence) (entity lod-entity))
  (setf (lods entity) (map 'vector #'identity lods)))

(defmethod select-lod ((lod lod) (entity lod-entity))
  (setf (mesh entity) (mesh lod)))

(defmethod select-lod ((level integer) (entity lod-entity))
  (let ((lods (lods entity)))
    (when (< 0 (length lods))
      (select-lod (aref lods (clamp 0 level (1- (length lods)))) entity))))

(defmethod select-lod ((value single-float) (entity lod-entity))
  (let ((lods (lods entity)))
    (when (< 0 (length lods))
      (loop with optimal-lod = (aref lods 0)
            for i from 1 below (length lods)
            for lod = (aref lods i)
            do (unless (< value (threshold lod))
                 (select-lod optimal-lod entity)
                 (return optimal-lod))
               (setf optimal-lod lod)))))

(defclass coverage-lod-entity (lod-entity)
  ())

;; KLUDGE: early def to avoid dependency cycle
(defclass camera () ())

(defmethod select-lod ((camera camera) (entity coverage-lod-entity))
  (select-lod (float (/ (screen-area entity camera) (screen-area NIL camera)) 0f0) entity))

(defclass distance-lod-entity (lod-entity)
  ())

(defmethod select-lod ((camera camera) (entity distance-lod-entity))
  (select-lod (vdistance (location entity) (location camera)) entity))
