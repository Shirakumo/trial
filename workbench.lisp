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
  ((line-width :initarg :line-width :initform 3.0 :accessor line-width))
  (:inhibit-shaders (vertex-entity :vertex-shader)))

(defclass line-vertex (vertex)
  ())

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

(defmethod paint :before ((lines lines) (target shader-pass))
  (let ((program (shader-program-for-pass target lines)))
    (setf (uniform program "line_width") (float (line-width lines)))
    (setf (uniform program "view_size") (vec (width *context*) (height *context*)))))

(define-class-shader (lines :vertex-shader)
  "layout(location = 0) in vec3 position;
layout(location = 1) in vec3 direction;

out vec2 line_normal;
uniform float line_width = 3.0;
uniform vec2 view_size = vec2(800,600);
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  float aspect = view_size.x/view_size.y;
  mat4 PVM = projection_matrix * view_matrix * model_matrix;
  vec4 screen1 = PVM * vec4(position, 1);
  vec4 screen2 = PVM * vec4(position+direction, 1);
  vec2 clip1 = screen1.xy/screen1.w;
  vec2 clip2 = screen2.xy/screen2.w;
  clip1.x *= aspect;
  clip2.x *= aspect;
  line_normal = normalize(clip2 - clip1);
  line_normal = vec2(-line_normal.y, line_normal.x);
  line_normal.x /= aspect;
  gl_Position = screen1 + screen1.w * vec4(line_normal*line_width/view_size, 0, 0);
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
