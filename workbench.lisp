(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.3 0.3 0.3 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench grid) mesh
    (make-line-grid 10 200 200))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)))

(define-shader-entity lines (vertex-entity)
  ()
  (:inhibit-shaders (vertex-entity :vertex-shader)))

(defclass line-vertex (vertex)
  ((next :initarg)))

(defmethod initialize-instance :after ((lines lines) &key points)
  (let ((mesh (make-instance 'vertex-mesh :vertex-type 'normal-vertex)))
    (with-vertex-filling (mesh)
      (loop for (a b) on points
            while b
            do (vertex :location a :normal (v- a b))
               (vertex :location b :normal (v- a b))
               (vertex :location a :normal (v- b a))
               (vertex :location b :normal (v- a b))
               (vertex :location b :normal (v- b a))
               (vertex :location a :normal (v- b a))))
    (setf (vertex-array lines) (change-class mesh 'vertex-array :vertex-form :triangles))))

(define-class-shader (lines :vertex-shader)
  "layout(location = 0) in vec3 position;
layout(location = 1) in vec3 direction;

out vec2 line_normal;
uniform float line_width = 3.0;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  mat4 PVM = projection_matrix * view_matrix * model_matrix;
  vec4 screen1 = PVM * vec4(position, 1);
  vec4 screen2 = PVM * vec4(position+direction, 1);
  line_normal = normalize(screen2.xy/screen2.w-screen1.xy/screen1.w);
  line_normal = vec2(-line_normal.y, line_normal.x);
  gl_Position = screen1 + vec4(line_normal*line_width/2, 0, 0);
}")

(define-class-shader (lines :fragment-shader)
  "in vec2 line_normal;
uniform float feather = 0.2;
out vec4 color;

void main(){
   color *= (1-length(line_normal))*16;
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'lines :points (list (vec 0 0 0) (vec 100 100 0) (vec 100 0 0))) scene)
    (enter (make-instance 'editor-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
