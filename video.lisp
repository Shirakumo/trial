(in-package #:org.shirakumo.fraf.trial)

(defclass video ()
  ((generator :initarg :generator :accessor generator)))

;; (defgeneric width (video))
;; (defgeneric height (video))
;; (defgeneric update (video dt tt fc))
;; (defgeneric clock (video))
(defgeneric duration (video))
(defgeneric seek (video to))
(defgeneric done-p (video))
(defgeneric framerate (video))

(defmethod resource ((video video) name)
  (resource (generator video) name))

;;; YUV video only for now.
(define-shader-entity video-display (vertex-entity)
  ((textures :initform NIL :accessor textures)
   (asset :initarg :asset :initform NIL)
   (video :accessor video)))

(defmethod shared-initialize :after ((entity video-display) slots &key video)
  (when video (setf (video entity) video)))

(defmethod (setf video) :after ((video video) (entity video-display))
  (setf (textures entity) (list (resource video :y) (resource video :u) (resource video :v)))
  (setf (vertex-array entity) (resource video :mesh)))

(defmethod stage :after ((entity video-display) (area staging-area))
  (when (slot-value entity 'asset)
    (register-load-observer area entity (slot-value entity 'asset))
    (stage (slot-value entity 'asset) area))
  (stage (textures entity) area))

(defmethod observe-load-state ((entity video-display) asset (state (eql :loaded)) (area staging-area))
  (setf (video entity) (video asset))
  (restage entity area))

(defmethod bind-textures :after ((entity video-display))
  (destructuring-bind (y u v) (textures entity)
    (bind y :texture0)
    (bind u :texture1)
    (bind v :texture2)))

(defmethod render :before ((entity video-display) (program shader-program))
  (setf (uniform program "Y_plane") 0)
  (setf (uniform program "U_plane") 1)
  (setf (uniform program "V_plane") 2))

(defmethod clock ((entity video-display))
  (clock (video entity)))

(defmethod duration ((entity video-display))
  (duration (video entity)))

(defmethod seek ((entity video-display) to)
  (seek (video entity) to))

(defmethod done-p ((entity video-display))
  (done-p (video entity)))

(define-class-shader (video-display :vertex-shader)
  "layout (location = TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;

void main(){
  maybe_call_next_method();
  uv = in_uv;
}")

(define-class-shader (video-display :fragment-shader)
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

(define-shader-entity video-player (video-display listener)
  ((state :initarg :state :initform :paused :accessor state)
   (loop-p :initform NIL :accessor loop-p)))

(define-handler ((entity video-player) tick) (tt dt fc)
  (when (eql :playing (state entity))
    (update (video entity) tt dt fc)
    (when (done-p entity)
      (typecase (loop-p entity)
        (integer
         (if (<= 0 (decf (loop-p entity)))
             (seek entity 0.0)
             (setf (state entity) :paused)))
        (null
         (setf (state entity) :paused))
        (T
         (seek entity 0.0))))))
