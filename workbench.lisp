(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 1 1 1 0)))

(define-pool workbench
  :base 'trial)

(defclass msdf-font (gl-asset texture)
  ((data :accessor data)))

(defmethod load ((font msdf-font))
  (let ((data (3b-bmfont:read-bmfont (input* font))))
    (setf (data font) data)
    (setf (internal-format font)
          (cond ((3b-bmfont:alpha-chnl data) :RGBA)
                ((3b-bmfont:blue-chnl data) :RGB)
                ((3b-bmfont:green-chnl data) :RG)
                ((3b-bmfont:red-chnl data) :R)
                (T (error "WTF"))))
    ;; FIXME: Multiple pages?
    (let ((file (merge-pathnames (getf (aref (3b-bmfont:pages data) 0) :file) (input* font))))
      (multiple-value-bind (data width height pixel-type pixel-format)
          (load-image file T)
        (setf (width font) width)
        (setf (height font) height)
        (setf (pixel-data font) data)
        (setf (pixel-type font) pixel-type)
        (setf (pixel-format font) pixel-format)
        (allocate font)))))

(defmethod render-text ((text string) (vbo vertex-buffer) (font msdf-font))
  (let ((mesh (make-instance 'vertex-mesh :vertex-type 'trial:textured-vertex)))
    (with-vertex-filling (mesh)
      (labels ((thunk (x- y- x+ y+ u- v- u+ v+)
                 (vertex :location (vec2 x- y+) :uv (vec2 u- (- 1 v-)))
                 (vertex :location (vec2 x- y-) :uv (vec2 u- (- 1 v+)))
                 (vertex :location (vec2 x+ y+) :uv (vec2 u+ (- 1 v-)))
                 (vertex :location (vec2 x+ y+) :uv (vec2 u+ (- 1 v-)))
                 (vertex :location (vec2 x- y-) :uv (vec2 u- (- 1 v+)))
                 (vertex :location (vec2 x+ y-) :uv (vec2 u+ (- 1 v+)))))
        (3b-bmfont:map-glyphs (data font) #'thunk text :y-up T)))
    (replace-vertex-data (buffer-data vbo) mesh)
    (when (allocated-p vbo)
      (resize-buffer vbo T))
    (length (vertices mesh))))

(define-shader-entity msdf-text (vertex-entity textured-entity readied)
  ((text :initarg :text :accessor text)
   (texture :initarg :font :accessor font))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(defmethod initialize-instance :after ((text msdf-text) &key)
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (make-array 0 :adjustable T :element-type 'single-float))))
    (setf (vertex-array text) (make-instance 'vertex-array :bindings `((,vbo :size 2 :offset 0 :stride 16)
                                                                       (,vbo :size 2 :offset 8 :stride 16))
                                                           :vertex-form :triangles))))

(defmethod (setf text) :after ((string string) (text msdf-text))
  (let ((verts (render-text string (caar (bindings (vertex-array text))) (font text))))
    (setf (size (vertex-array text)) verts)
    string))

(defmethod resources-ready ((text msdf-text))
  (setf (text text) (text text)))

(define-class-shader (msdf-text :fragment-shader)
  "in vec2 texcoord;
uniform sampler2D texture_image;
uniform float pxRange = 4;
uniform vec4 bgColor = vec4(1, 0, 0, 0.5);
uniform vec4 fgColor = vec4(0, 0, 0, 1);

out vec4 color;

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}

void main() {
    vec2 msdfUnit = pxRange/vec2(textureSize(texture_image, 0));
    vec3 msdfData = texture(texture_image, texcoord).rgb;
    float sigDist = median(msdfData.r, msdfData.g, msdfData.b) - 0.5;
    sigDist *= dot(msdfUnit, 0.5/fwidth(texcoord));
    float opacity = clamp(sigDist + 0.5, 0.0, 1.0);
    color = mix(bgColor, fgColor, opacity);
}")

(define-asset (workbench font) msdf-font
    #p"NotoMono-Regular.fnt")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'msdf-text :font (asset 'workbench 'font) :text "Thanks for all your good work, |3b|!") scene)eo
    (enter (make-instance 'editor-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
