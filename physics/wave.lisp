(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass wave-propagate-pass (temporal-post-effect-pass)
  ((energy-compensation :initarg :energy-compensation :initform 0.128 :uniform T :accessor energy-compensation)
   (propagation-speed :initarg :propagation-speed :initform 0.25 :uniform T :accessor propagation-speed)
   (oscillator-speed :initarg :oscillator-speed :initform 0.001 :uniform T :accessor oscillator-speed)))

(defmethod initialize-instance :after ((pass wave-propagate-pass) &key (resolution 64) (width resolution) (height resolution))
  (setf (previous pass) (make-instance 'texture :target :texture-2d :internal-format :rg32f :width width :height height))
  (setf (color pass) (make-instance 'texture :target :texture-2d :internal-format :rg32f :width width :height height))
  (setf (framebuffer pass) (make-instance 'framebuffer :attachments `((:color-attachment0 ,(color pass))) :clear-bits ())))

(defmethod resize ((pass wave-propagate-pass) width height)
  (resize (previous pass) width height)
  (resize (color pass) width height)
  (resize (framebuffer pass) width height))

(defmethod clear ((pass wave-propagate-pass))
  (clear (color pass))
  (clear (previous pass)))

(defmethod update ((pass wave-propagate-pass) dt tt fc)
  (render pass NIL))

(define-class-shader (wave-propagate-pass :fragment-shader)
  "uniform sampler2D previous;
uniform float energy_compensation = 0.128;
uniform float propagation_speed = 0.25;
uniform float oscillator_speed = 0.001;
in vec2 uv;
out vec4 color;

void main(){
  vec2 current = texture(previous, uv).rg;
  float previous_height = current.r;
  current.r += (current.r-current.g);
  current.g = previous_height;
  current.r *= 1.0-oscillator_speed;
  current.r += (current.r-current.g)*energy_compensation;
  float local_sum = textureOffset(previous, uv, ivec2(-1, 0)).r
                  + textureOffset(previous, uv, ivec2(+1, 0)).r
                  + textureOffset(previous, uv, ivec2(0, -1)).r
                  + textureOffset(previous, uv, ivec2(0, +1)).r;
  current.r += (local_sum*0.25-current.r)*propagation_speed*0.5;
  color = vec4(current.rg,0,1);
}")

(defmethod enter ((pos vec4) (pass wave-propagate-pass))
  ;; The POS is: (U V RADIUS AMPLITUDE)
  (let* ((w (width pass))
         (h (height pass))
         (r (vz pos))
         (s (* r (max w h)))
         (x (* w (vx pos)))
         (y (* h (vy pos)))
         (a (vw pos))
         (i -1)
         (x- (max (floor (- x s)) 0)) (x+ (min (ceiling (+ x s)) (- w 1)))
         (y- (max (floor (- y s)) 0)) (y+ (min (ceiling (+ y s)) (- w 1)))
         (w (- x+ x-)) (h (- y+ y-)))
    (when (and (< 0 w) (< 0 h))
      (let ((array (make-array (* 2 w h) :element-type 'single-float)))
        (declare (dynamic-extent array))
        (loop for ix from x- below x+
              do (loop for iy from y- below y+
                       ;; We draw a radial gradient around the place we want to poke
                       for d = (max 0.0 (- 1 (/ (sqrt (+ (expt (- x ix) 2) (expt (- y iy) 2))) 2 s)))
                       do (setf (aref array (incf i)) (* d a))
                          (setf (aref array (incf i)) 0.0)))
        (ignore-errors (update-buffer-data (previous pass) array :x x- :y y- :width w :height h))
        (gl:generate-mipmap :texture-2d)))))

(defmethod enter ((pos vec3) (pass wave-propagate-pass))
  (enter (vec (vx pos) (vy pos) (vz pos) 2) pass))

(defmethod enter ((pos vec2) (pass wave-propagate-pass))
  (enter (vec (vx pos) (vy pos) (/ 2 (width pass)) 2) pass))

(defmethod clear ((pass wave-propagate-pass))
  (clear (previous pass)))
