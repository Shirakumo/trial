(in-package #:org.shirakumo.fraf.trial.examples)

(define-example spectrogram
  :title "Spectrogram"
  :description "Live audio spectrogram using microphone input."
  (trial-harmony:initialize-audio-backend NIL NIL :source T :mixers '() :start NIL)
  (let* ((visualizer (make-instance 'spectrogram-visualizer :framesize 8192 :frame-count 1024))
         (input (harmony:segment :input harmony:*server*))
         (fft (make-instance 'mixed:fwd-fft :samplerate (harmony:samplerate harmony:*server*)
                                            :framesize (framesize visualizer)))
         (seg (make-instance 'spectrogram-segment :spectrogram visualizer)))
    (harmony:connect input T fft T)
    (mixed:connect fft 0 seg 0 (mixed:make-buffer (mixed:framesize fft)))
    (harmony:add-to input fft seg)
    (enter visualizer scene)
    (enter (make-instance 'render-pass) scene)
    (harmony:start harmony:*server*)))

(defmethod finalize :before ((scene spectrogram-scene))
  (when harmony:*server*
    (trial:finalize harmony:*server*)))

(defclass spectrogram-segment (mixed:virtual)
  ((spectrogram :initarg :spectrogram :accessor spectrogram)))

(defmethod mixed:info ((segment spectrogram-segment))
  (list :name "spectrogram-segment"
        :description "Performs a spectrogram analysis."
        :flags 0 :min-inputs 1 :max-inputs 1 :outputs 0 :fields ()))

(defmethod mixed:mix ((segment spectrogram-segment))
  (declare (optimize speed (safety 1)))
  (mixed:with-buffer-tx (data start size (aref (mixed:inputs segment) 0))
    (declare (type (simple-array single-float (*)) data))
    (when (< 0 size)
      (let* ((visualizer (spectrogram segment))
             (i (i visualizer))
             (spectrogram (spectrogram visualizer)))
        (declare (type (simple-array single-float (*)) spectrogram))
        (declare (type (unsigned-byte 32) i))
        (replace spectrogram data :start1 i :start2 start :end2 (+ start size))
        (setf (i visualizer) (mod (+ i size) (length spectrogram))))
      (mixed:finish))))

(define-shader-entity spectrogram-visualizer (vertex-entity textured-entity)
  ((gradient :initform (assets:// :intensity-gradient) :accessor gradient)
   (texture :accessor texture)
   (vertex-array :initform (// 'trial 'fullscreen-square))
   (spectrogram :accessor spectrogram)
   (framesize :initarg :framesize :initform 2048 :accessor framesize)
   (range :initarg :range :initform '(100 . 8000) :accessor range)
   (magnitude-scale :uniform T :initarg :magnitude-scale :initform 1.0 :accessor magnitude-scale)
   (magnitude-offset :uniform T :initarg :magnitude-offset :initform 0.0 :accessor magnitude-offset)
   (frequency-min :uniform T :initarg :frequency-min :initform 0.0 :accessor frequency-min)
   (frequency-max :uniform T :initarg :frequency-max :initform 22049.0 :accessor frequency-max)
   (i :initform 0 :accessor i)
   (last-i :initform 0 :accessor last-i))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(defmethod initialize-instance :after ((visualizer spectrogram-visualizer) &key (frame-count 1024))
  (let* ((framesize (framesize visualizer))
         (spectrogram (make-array (* framesize frame-count) :element-type 'single-float)))
    (setf (spectrogram visualizer) spectrogram)
    (setf (texture visualizer) (make-instance 'texture
                                              :pixel-data spectrogram
                                              :internal-format :rg32f
                                              :width (/ framesize 2)
                                              :height (truncate (length spectrogram) framesize)
                                              :wrapping '(:clamp-to-edge :repeat :clamp-to-edge)
                                              :min-filter :linear
                                              :mag-filter :linear))))

(defmethod stage :after ((visualizer spectrogram-visualizer) (area staging-area))
  (stage (texture visualizer) area)
  (stage (gradient visualizer) area))

(defmethod render :before ((visualizer spectrogram-visualizer) (program shader-program))
  (let ((i (i visualizer))
        (spectrogram (spectrogram visualizer)))
    (setf (uniform program "gradient") (bind (gradient visualizer) 1))
    (setf (uniform program "spectrogram") (bind (texture visualizer) 0))
    (setf (uniform program "view_size") (size *context*))
    ;; FIXME: only update the actually changed partrs of the texture.
    (update-buffer-data (texture visualizer) T)
    (setf (last-i visualizer) i)
    (setf (frequency-max visualizer) 8000.0)
    ;; TODO: compute the correct scaling and offset to view the range
    (setf (uniform program "offset") (vec2 0 (/ i (length spectrogram))))))

(define-class-shader (spectrogram-visualizer :fragment-shader)
  "uniform sampler2D spectrogram;
uniform sampler2D gradient;
uniform vec2 offset = vec2(0);
uniform vec2 view_size = vec2(1);
uniform float magnitude_scale = 1.0;
uniform float magnitude_offset = 0.0;
uniform float frequency_min = 0.0;
uniform float frequency_max = 22049.0;
in vec2 uv;
out vec4 color;

void main(){
  float uvy = (pow(uv.y,2) * (frequency_max - frequency_min) + frequency_min) / 22049;
  vec2 pos = vec2(uvy, uv.x)+offset;
  // Read out the values
  vec2 freqmag = texture(spectrogram, pos).rg;
  float freq = freqmag.x;
  float mag = freqmag.y*magnitude_scale+magnitude_offset;
  // Do a gradient mapping of the intensity
  vec3 intensity = texture(gradient, vec2(mag, 0.5)).rgb;
  // Figure out lines
  freq = pos.x * 22049;
  float nextfreq = freq + dFdy(freq);
  if(floor(freq / 1000) != floor(nextfreq / 1000)) intensity = mix(intensity, vec3(1), 0.8);
  else if(floor(freq / 500) != floor(nextfreq / 500)) intensity = mix(intensity, vec3(1), 0.25);
  color = vec4(intensity, 1);
}")

