(in-package #:org.shirakumo.fraf.trial.examples)

(define-asset (examples wave-grid) mesh
    (make-quad-grid-mesh 1 64 64))

(define-shader-entity wave-simulation (vertex-entity listener)
  ((wave-pass :initform (make-instance 'wave-propagate-pass) :accessor wave-pass)
   (vertex-array :initform (// 'examples 'wave-grid)))
  (:inhibit-shaders (vertex-entity :vertex-shader)))

(defmethod stage :after ((simulation wave-simulation) (area staging-area))
  (stage (wave-pass simulation) area))

(define-handler (wave-simulation tick) (tt dt fc)
  (when (= 0 (mod fc 50))
    (enter (vrand (vec2 0.5) 1.0) (wave-pass wave-simulation)))
  (update (wave-pass wave-simulation) tt dt fc))

(defmethod bind-textures :after ((simulation wave-simulation))
  (bind (color (wave-pass simulation)) :texture0))

(define-class-shader (wave-simulation :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 2) in vec2 uv;

uniform sampler2D heightmap;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
out float height;

void main(){
  height = texture(heightmap, uv).r;
  vec4 local_position = vec4(vec3(position.x, position.y+height, position.z), 1.0f);
  gl_Position = projection_matrix * view_matrix * model_matrix * local_position;
}")

(define-class-shader (wave-simulation :fragment-shader)
  "
in float height;
void main(){
  color = vec4(vec3(1)*height, 1);
}")

(define-example wave
  :title "Wave simulation"
  :description "A demo on how to use Trial's 2D wave simulation to render animated water surfaces."
  (enter (make-instance 'wave-simulation) scene)
  (enter (make-instance 'editor-camera :location (vec 0 36 42) :rotation (vec 0.84 0.01 0) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'render-pass) scene))
