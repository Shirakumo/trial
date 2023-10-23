(in-package #:org.shirakumo.fraf.trial)

;;; YUV video only for now.

(define-shader-entity video (vertex-entity listener)
  ((textures :accessor textures)
   (asset :initarg :asset)
   (clock :initform 0f0 :reader clock)
   (frame :initform 0 :accessor frame)
   (state :initarg :state :initform :paused :accessor state)
   (loop-p :initform NIL :accessor loop-p)))

(defmethod shared-initialize :after ((video video) slots &key asset)
  (when asset
    (setf (textures video) (list (resource asset :y) (resource asset :u) (resource asset :v)))
    (setf (vertex-array video) (resource asset :mesh))))

(defmethod stage :after ((entity video) (area staging-area))
  (stage (textures entity) area))

(defmethod bind-textures :after ((entity video))
  (destructuring-bind (y u v) (textures entity)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (gl-name y))
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (gl-name u))
    (gl:active-texture :texture2)
    (gl:bind-texture :texture-2d (gl-name v))))

(defmethod render :before ((entity video) (program shader-program))
  (setf (uniform program "Y_plane") 0)
  (setf (uniform program "U_plane") 1)
  (setf (uniform program "V_plane") 2))

(defmethod (setf clock) (new (video video))
  (seek video new))

(defmethod duration ((video video))
  (duration (slot-value video 'asset)))

(defmethod seek ((video video) to)
  (setf (slot-value video 'clock) (seek (slot-value video 'asset) to))
  (setf (frame video) 0))

(defmethod done-p ((video video))
  (done-p (slot-value video 'asset)))

(define-handler (video tick) (dt)
  (when (eql :playing (state video))
    (let* ((asset (slot-value video 'asset))
           (tt (+ (clock video) dt))
           (fc (update asset tt dt (frame video))))
      (setf (slot-value video 'clock) tt)
      (setf (frame video) fc)
      (when (done-p asset)
        (if (loop-p video)
            (setf (clock video) 0.0)
            (setf (state video) :paused))))))

(define-class-shader (video :vertex-shader)
  "layout (location = 2) in vec2 in_uv;
out vec2 uv;

void main(){
  maybe_call_next_method();
  uv = in_uv;
}")

(define-class-shader (video :fragment-shader)
  "in vec2 uv;
out vec4 color;
uniform sampler2D Y_plane, U_plane, V_plane;
void main() {
  vec3 yuv = vec3(texture(Y_plane, vec2(uv.x, 1-uv.y)).r - 0.0625,
                  texture(U_plane, vec2(uv.x, 1-uv.y)).r - 0.5,
                  texture(V_plane, vec2(uv.x, 1-uv.y)).r - 0.5);
  vec3 rgb = vec3(dot(yuv, vec3(+1.164, +0.000, +1.793)),
                  dot(yuv, vec3(+1.164, -0.213, -0.533)),
                  dot(yuv, vec3(+1.164, +2.112, +0.000)));
  color = vec4(rgb, 1.0);
}")
