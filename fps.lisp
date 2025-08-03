(in-package #:org.shirakumo.fraf.trial)

;;;; Very fast FPS display. Focus is on reducing GPU load.

(define-asset (trial fps-counter) mesh
    (with-mesh-construction (v :attributes (location uv) :deduplicate NIL)
      (loop for x from 0 by 16
            repeat 6
            do (v (+ x 16) 16 0 0.1 1)
               (v (+ x 0)  16 0 0   1)
               (v (+ x 0)   0 0 0   0)
               (v (+ x 0)   0 0 0   0)
               (v (+ x 16)  0 0 0.1 0)
               (v (+ x 16) 16 0 0.1 1))
      (finalize-data))
  :data-usage :stream-draw)

(declaim (type (unsigned-byte 60) +frame-count+ +start-time+))
(define-global +frame-count+ 0)
(define-global +start-time+ 0)

(define-shader-entity fps-counter (renderable standalone-shader-entity)
  ((name :initform 'fps-counter)))

(defmethod stage :after ((counter fps-counter) (area staging-area))
  (stage (// 'trial 'fps-counter) area)
  (stage (// 'trial 'ascii) area))

(defmethod render ((counter fps-counter) (program shader-program))
  (declare (optimize speed (safety 1)))
  (let* ((vao (// 'trial 'fps-counter))
         (now (get-internal-real-time))
         (dt (- now +start-time+)))
    (incf +frame-count+)
    (when (<= (/ internal-time-units-per-second 10) dt)
      (let* ((fps (floor (/ +frame-count+ (/ dt internal-time-units-per-second))))
             (buf (caadr (bindings vao)))
             (dat (buffer-data buf)))
        (declare (type (simple-array single-float) dat))
        (declare (type (unsigned-byte 32) fps))
        (setf +start-time+ now)
        (setf +frame-count+ 0)
        (flet ((set-rect (i d)
                 (let* ((glyphs #.(truncate (fourth (texture-source-src (load-image (input* (asset 'trial 'ascii)) T))) 9))
                        (b (* 5 (* 6 i)))
                        (d0 (* (+ d 16) (/ 1.0 glyphs)))
                        (d1 (+ d0 (/ 1.0 glyphs))))
                   (setf (aref dat (+ b  3)) d1)
                   (setf (aref dat (+ b  8)) d0)
                   (setf (aref dat (+ b 13)) d0)
                   (setf (aref dat (+ b 18)) d0)
                   (setf (aref dat (+ b 23)) d1)
                   (setf (aref dat (+ b 28)) d1))))
          (loop for i downfrom 5 to 0
                for div = 1 then (* 10 div)
                do (set-rect i (mod (floor fps div) 10))))
        (update-buffer-data buf T)))
    (bind (// 'trial 'ascii) :texture0)
    (with-depth-mask T
      (render vao program))))

(define-class-shader (fps-counter :vertex-shader)
  "
layout (location = TRIAL_V_LOCATION) in vec3 position;
layout (location = TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;

void main(){
  gl_Position = mat4(0.0015625, 0.0, 0.0, 0.0, 0.0, 0.0027777778, 0.0, 0.0, 0.0, 0.0, -0.02, 0.0, -1.0, -1.0, -1.0, 1.0)
              * vec4(position, 1.0);
  uv = in_uv;
}")

(define-class-shader (fps-counter :fragment-shader)
  "uniform sampler2D texture_image;
in vec2 uv;
out vec4 color;

void main(){
  color = texture(texture_image, uv);
}")

(declaim (type (double-float 0d0) +last-process-time+ +last-gpu-time+ +last-gc-time+))
(declaim (type fixnum +last-io-bytes+))
(define-global +last-process-time+ 0d0)
(define-global +last-gpu-time+ 0d0)
(define-global +last-gc-time+ 0d0)
(define-global +last-io-bytes+ 0)
(define-asset (trial system-stats) static 'texture
  :pixel-data (make-array (* 6 100) :element-type '(unsigned-byte 8))
  :width 100 :height 6 :internal-format :red
  :min-filter :linear :mag-filter :linear
  :wrapping '(:repeat :repeat :repeat))
(define-asset (trial system-stats-mesh) mesh
    (append-vertex-data*
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.31 :y 0.49)
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.31 :y 0.49)
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.54 :y 0.49)
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.54 :y 0.49)
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.77 :y 0.49)
     (make-rectangle-mesh 0.22 0.5 :align :bottomleft :x 0.77 :y 0.49)))

(define-shader-entity system-stats (renderable standalone-shader-entity)
  ((name :initform 'system-stats)))

(defmethod stage :after ((stats system-stats) (area staging-area))
  (setf +last-process-time+ 0d0)
  (setf +last-gpu-time+ 0d0)
  (setf +last-gc-time+ 0d0)
  (setf +last-io-bytes+ 0)
  (stage (// 'trial 'system-stats) area)
  (stage (// 'trial 'system-stats-mesh) area))

(defmethod render ((stats system-stats) (program shader-program))
  (declare (optimize speed (safety 1)))
  (let* ((data (pixel-data (// 'trial 'system-stats))))
    (declare (type (simple-array (unsigned-byte 8) (*)) data))
    (loop for i from 0 below (1- (length data))
          do (setf (aref data i) (aref data (1+ i))))
    (labels ((update (i val)
               (setf (aref data (1- (* 100 i))) val))
             (compute-frac (free total)
               (if (= 0 total)
                   0
                   (- 255 (the (unsigned-byte 8) (round (the (unsigned-byte 64) (* 255 free)) total)))))
             (compute-bytes (db)
               ;; Rationale: one kilobyte should count as "max".
               (clamp 0 (round (the (unsigned-byte 64) (* 255 db)) 1024) 255))
             (compute-time (dt)
               ;; Rationale: if we take 1/60th of a second of time, that should count as "max".
               (clamp 0 (round (the (double-float 0d0) (* dt 60 255))) 255)))
      (declare (inline compute-time compute-frac))
      ;; 1: CPU Time
      (let ((time (org.shirakumo.machine-state:process-time)))
        (update 1 (compute-time (- time +last-process-time+)))
        (setf +last-process-time+ time))
      ;; 2: RAM (GC Space)
      (multiple-value-bind (free total) (org.shirakumo.machine-state:gc-room)
        (update 2 (compute-frac free total)))
      ;; 3: GPU Time
      (let ((time (org.shirakumo.machine-state:gpu-time)))
        (update 3 (compute-time (- time +last-gpu-time+)))
        (setf +last-gpu-time+ time))
      ;; 4: VRAM
      (multiple-value-bind (free total) (org.shirakumo.machine-state:gpu-room)
        (update 4 (compute-frac free total)))
      ;; 5: GC Time
      (let ((time (org.shirakumo.machine-state:gc-time)))
        (update 5 (compute-time (- time +last-gc-time+)))
        (setf +last-gc-time+ time))
      ;; 6: IO Bytes
      (let ((bytes (org.shirakumo.machine-state:process-io-bytes)))
        (update 6 (compute-bytes (- bytes +last-io-bytes+)))
        (setf +last-io-bytes+ bytes)))
    (update-buffer-data (// 'trial 'system-stats) data))
  (bind (// 'trial 'system-stats) :texture0)
  (setf (uniform program "pixel_scale") (/ 6.0 (min (height *context*) (width *context*))))
  (with-depth-mask T
    (render (// 'trial 'system-stats-mesh) program)))

(define-class-shader (system-stats :vertex-shader)
  "
layout (location = TRIAL_V_LOCATION) in vec3 position;
layout (location = TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;
out vec3 graph_color;
out float stat;

const vec3 colors[] = vec3[](
  vec3(1.0, 0.0, 0.0), // CPU Time
  vec3(0.8, 0.5, 0.2), // RAM
  vec3(0.0, 1.0, 0.0), // GPU Time
  vec3(0.2, 0.8, 0.5), // VRAM
  vec3(1.0, 1.0, 1.0), // GC Pause
  vec3(0.0, 0.0, 1.0), // IO Bytes
);

void main(){
  int s = gl_VertexID / 4;
  vec2 pos = vec2(position.x, position.y);
  gl_Position = vec4(pos, -1.0, 1.0);
  stat = float(s+0.5)/6;
  graph_color = colors[s];
  uv = in_uv;
}")

(define-class-shader (system-stats :fragment-shader)
  "uniform sampler2D texture_image;
in vec2 uv;
in vec3 graph_color;
in float stat;
out vec4 color;
const int stats = 6;
uniform float pixel_scale = 0.01;

float line_sdf(in vec2 p, in vec2 a, in vec2 b){
  vec2 pa = p-a, ba = b-a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return length( pa - ba*h );
}

void main(){
  color = vec4(0.2);
  if(uv.x < pixel_scale || uv.y < pixel_scale ||
     1-pixel_scale < uv.x || 1-pixel_scale < uv.y)
    color = vec4(1,1,1,0.3);

  float y = texture(texture_image, vec2(uv.x, stat)).r;
  y = uv.y - y;
  // Draw the line
  float sdf = abs(y) - pixel_scale;
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);
  color = mix(color, vec4(graph_color, sdf), sdf);
  // Draw the fill
  sdf = (y < 0) ? max(0,y+0.5) : 0.0;
  color = mix(color, vec4(graph_color, 1), sdf);
}")
