(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct parallax
  (parallax :vec2 :accessor parallax)
  (scaling :vec2 :accessor scaling)
  (offset :vec2 :accessor offset)
  (view-size :vec2 :accessor view-size))

(define-asset (trial parallax) uniform-block
    'parallax)

(define-shader-entity parallax-background (renderable listener)
  ((name :initform 'parallax-background)
   (texture :initform NIL :accessor texture)
   (default-background :initarg :default-background :initform () :accessor default-background))
  (:buffers (trial parallax)))

(defmethod render ((parallax-background parallax-background) (program shader-program))
  (unless (texture parallax-background)
    ;; Set up default here, I guess.
    (apply #'change-background
           (append (default-background parallax-background)
                   (list :unit parallax-background
                         :scaling (vec 1 1)
                         :offset (vec 0 0)
                         :parallax (vec 2 2)))))
  (setf (uniform program "view_matrix") (minv *view-matrix*))
  (bind (texture parallax-background) :texture0)
  (render-array (// 'trial 'empty-vertex-array) :vertex-form :triangle-strip :vertex-count 4))

(defmethod handle ((ev resize) (parallax-background parallax-background))
  (with-buffer-tx (bg (// 'trial 'parallax))
    (setf (view-size bg) (vec2 (max 1 (width ev)) (max 1 (height ev))))))

(defmethod stage :after ((entity parallax-background) (area staging-area))
  (stage (// 'trial:trial 'trial::empty-vertex-array) area)
  (when (texture entity)
    (stage (texture entity) area)))

(defun change-background (&key texture scaling parallax offset unit)
  (when texture
    (setf (texture (or unit (node 'parallax-background (scene +main+)))) texture))
  (when (or parallax scaling offset)
    (with-buffer-tx (bg (// 'trial 'parallax))
      (when parallax
        (setf (parallax bg) parallax))
      (when scaling
        (setf (scaling bg) scaling))
      (when offset
        (setf (offset bg) offset)))))

(define-class-shader (parallax-background :vertex-shader)
  (gl-source (asset 'trial 'parallax))
  "const vec2 quad_vertices[4] = vec2[4](vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0), vec2(1.0, 1.0));

uniform sampler2D tex_image;
uniform mat4 view_matrix;

out vec2 map_coord;
void main(){
  gl_Position = vec4(quad_vertices[gl_VertexID], 0, 1);
  vec2 vertex_uv = (quad_vertices[gl_VertexID]+1)*0.5;
  vec2 tex_size = textureSize(tex_image, 0).xy;
  map_coord = (view_matrix * vec4(vertex_uv*parallax.view_size*parallax.parallax, 0, 1)).xy;
  map_coord += tex_size/2 + parallax.offset;
  map_coord /= parallax.parallax * parallax.scaling * tex_size;
}")

(define-class-shader (parallax-background :fragment-shader)
  "uniform sampler2D tex_image;
in vec2 map_coord;
out vec4 color;
void main(){
  color = texture(tex_image, map_coord);
}")
