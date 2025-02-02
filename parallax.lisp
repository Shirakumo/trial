(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity parallax-background (renderable listener)
  ((name :initform 'parallax-background)
   (texture :initform NIL :initarg :texture :accessor texture)
   (parallax :initform (vec 2 2) :initarg :parallax :uniform T :accessor parallax)
   (scaling :initform (vec 1 1) :initarg :scaling :uniform T :accessor scaling)
   (offset :initform (vec 0 0) :initarg :offset :uniform T :accessor offset)
   (view-size :initform (vec 1 1) :initarg :view-size :uniform T :accessor view-size)))

(defmethod render ((entity parallax-background) (program shader-program))
  (vsetf (view-size entity) (max 1 (width *context*)) (max 1 (height *context*)))
  (setf (uniform program "view_matrix") (minv *view-matrix*))
  (bind (texture entity) :texture0)
  (render-array (// 'trial 'fullscreen-square)))

(defmethod in-view-p ((entity parallax-background) camera) T)

(defmethod handle ((ev resize) (entity parallax-background))
  (vsetf (view-size entity) (max 1 (width ev)) (max 1 (height ev))))

(defmethod stage :after ((entity parallax-background) (area staging-area))
  (stage (// 'trial 'fullscreen-square) area)
  (when (texture entity)
    (stage (texture entity) area)
    (setf (wrapping (texture entity)) '(:repeat :repeat :repeat))))

(defmethod (setf texture) :before ((texture texture) (entity parallax-background))
  (setf (wrapping texture) '(:repeat :repeat :repeat)))

(defmethod (setf background) ((data cons) (entity parallax-background))
  (destructuring-bind (&key texture scaling parallax offset) data
    (when texture
      (setf (texture entity) texture))
    (when parallax
      (setf (parallax entity) parallax))
    (when scaling
      (setf (scaling entity) scaling))
    (when offset
      (setf (offset entity) offset))))

(define-class-shader (parallax-background :vertex-shader)
  "
layout (location = TRIAL_V_LOCATION) in vec3 position;
layout (location = TRIAL_V_UV) in vec2 in_uv;

uniform vec2 parallax = vec2(1,1);
uniform vec2 scaling = vec2(1,1);
uniform vec2 offset = vec2(0,0);
uniform vec2 view_size = vec2(1,1);
uniform sampler2D tex_image;
uniform mat4 view_matrix;

out vec2 map_coord;
void main(){
  gl_Position = vec4(position.xy, +1, 1);
  vec2 tex_size = textureSize(tex_image, 0).xy;
  map_coord = (view_matrix * vec4(in_uv*view_size*parallax, 0, 1)).xy;
  map_coord += tex_size/2 + offset;
  map_coord /= parallax * scaling * tex_size;
}")

(define-class-shader (parallax-background :fragment-shader)
  "uniform sampler2D tex_image;
in vec2 map_coord;
out vec4 color;
void main(){
  color = texture(tex_image, map_coord);
}")
