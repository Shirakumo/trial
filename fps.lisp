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

(define-shader-entity fps-counter (renderable)
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

(define-shader-entity system-stats (renderable)
  ((name :initform 'system-stats)))

(defmethod stage :after ((stats system-stats) (area staging-area))
  (stage (// 'trial 'system-stats) area)
  (stage (// 'trial 'unit-square) area))

(defmethod render ((stats system-stats) (program shader-program))
  (declare (optimize speed (safety 1)))
  (let* ((data (pixel-data (// 'trial 'system-stats))))
    (declare (type (simple-array (unsigned-byte 8) (*)) data))
    (loop for i from 0 below (1- (length data))
          do (setf (aref data i) (aref data (1+ i))))
    (labels ((update (i val)
               (setf (aref data (1- (* 100 i))) val))
             (compute-frac (free total)
               (- 255 (the (unsigned-byte 8) (round (the (unsigned-byte 64) (* 255 free)) total))))
             (compute-bytes (db)
               ;; Rationale: one kilobyte should count as "max".
               (clamp 0 (round (the (unsigned-byte 64) (* 255 db)) 255) 255))
             (compute-time (dt)
               ;; Rationale: if we take 1/60th of a second of time, that should count as "max".
               (clamp 0 (round (the (double-float 0d0) (* dt 60 255))) 255)))
      (declare (inline update-time update-frac))
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
  (with-depth-mask T
    (render (// 'trial 'unit-square) program)))

(define-class-shader (system-stats :vertex-shader)
  "
layout (location = TRIAL_V_LOCATION) in vec3 position;
layout (location = TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;

void main(){
  gl_Position = vec4(position.xy*0.5+0.75, -1.0, 1.0);
  uv = in_uv;
}")

(define-class-shader (system-stats :fragment-shader)
  "uniform sampler2D texture_image;
in vec2 uv;
out vec4 color;
const int stats = 6;
const float line_thickness = 0.01;

const vec3 colors[] = vec3[](
  vec3(1.0, 0.0, 0.0), // CPU Time
  vec3(0.8, 0.5, 0.2), // RAM
  vec3(0.0, 1.0, 0.0), // GPU Time
  vec3(0.2, 0.8, 0.5), // VRAM
  vec3(1.0, 1.0, 1.0), // GC Pause
  vec3(0.2, 0.8, 0.1), // IO Bytes
);

void draw_line(int s){
  float y = texture(texture_image, vec2(uv.x, float(s+0.5)/stats)).r;
  float sdf = abs(y - uv.y) - line_thickness;
  float dsdf = fwidth(sdf)*0.5;
  sdf = smoothstep(dsdf, -dsdf, sdf);
  color = mix(color, vec4(colors[s], sdf), sdf);
}

void draw_fill(int s){
  float y = texture(texture_image, vec2(uv.x, float(s+0.5)/stats)).r;
  float sdf = (uv.y < y) ? 0.2: 0.0;
  color = mix(color, vec4(colors[s], 1.0), sdf);
}

void main(){
  color = vec4(0);
  draw_fill(5);
  draw_fill(4);
  draw_fill(3);
  draw_fill(2);
  draw_fill(1);
  draw_fill(0);

  draw_line(5);
  draw_line(4);
  draw_line(3);
  draw_line(2);
  draw_line(1);
  draw_line(0);
}")
