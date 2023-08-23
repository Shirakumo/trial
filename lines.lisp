(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity lines (vertex-entity)
  ((line-width :initarg :line-width :initform 3.0 :accessor line-width)
   (vertex-array :initform NIL))
  (:inhibit-shaders (vertex-entity :vertex-shader)))

(defmethod initialize-instance :after ((lines lines) &key points)
  (unless (vertex-array lines)
    (setf (vertex-array lines) (generate-resources 'mesh-loader (make-lines points) :data-usage :dynamic-draw))))

(defmethod replace-vertex-data ((lines lines) points &key (update T) (default-color (vec 0 0 0 1)))
  (replace-vertex-data (vertex-array lines) (make-lines points :default-color default-color) :update update)
  (setf (size (vertex-array lines)) (* 3 (length points))))

(defmethod render :before ((lines lines) (program shader-program))
  (setf (uniform program "line_width") (float (line-width lines)))
  (setf (uniform program "view_size") (vec (width *context*) (height *context*))))

(define-class-shader (lines :vertex-shader)
  "layout(location = 0) in vec3 position;
layout(location = 1) in vec3 direction;
layout(location = 2) in vec4 color;

out vec2 line_normal;
out vec4 line_color;
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
  vec2 clip1 = screen1.xy / max(0.00001, screen1.w);
  vec2 clip2 = screen2.xy / max(0.00001, screen2.w);
  clip1.x *= aspect;
  clip2.x *= aspect;
  line_normal = normalize(clip2 - clip1);
  line_normal = vec2(-line_normal.y, line_normal.x);
  gl_Position = screen1 + screen1.w * vec4(line_normal*line_width/view_size, 10, 0);
  line_color = color;
}")

(define-class-shader (lines :fragment-shader)
  "in vec2 line_normal;
in vec4 line_color;
uniform float feather = 0.3;
out vec4 color;

void main(){
   color = line_color*((1-length(line_normal))/feather);
}")
