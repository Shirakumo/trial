#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *default-clipmap-resolution* 128)

(define-shader-entity geometry-clipmap (located-entity)
  ((previous-update-location :initform (vec2 most-positive-single-float most-positive-single-float)
                             :accessor previous-update-location)
   (current-height :accessor current-height)
   (mmap-cache :accessor mmap-cache)
   (clipmap-block :accessor clipmap-block)
   (levels :initarg :levels :accessor levels)
   (resolution :initarg :resolution :accessor resolution)
   (map-scale :initarg :map-scale :accessor map-scale)
   (height-map :accessor height-map)
   (splat-map :accessor splat-map)
   (data-directory :initarg :data-directory :accessor data-directory))
  (:default-initargs
   :levels 5
   :map-scale (vec 1 1 1)
   :resolution *default-clipmap-resolution*
   :data-directory (error "DATA-DIRECTORY required.")))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key levels resolution data-directory)
  (setf (data-directory clipmap) (uiop:native-namestring (make-pathname :name NIL :type NIL :defaults data-directory)))
  (setf (mmap-cache clipmap) (make-array levels :initial-element ()))
  (setf (clipmap-block clipmap) (make-clipmap-block resolution levels))
  (setf (height-map clipmap) (make-instance 'texture :target :texture-2d-array
                                                     :min-filter :linear
                                                     :pixel-format :red
                                                     :pixel-type :unsigned-short
                                                     :internal-format :r16
                                                     :width resolution
                                                     :height resolution
                                                     :depth levels
                                                     :storage :static))
  (setf (splat-map clipmap) (make-instance 'texture :target :texture-2d-array
                                                    :min-filter :linear
                                                    :pixel-format :rgba
                                                    :pixel-type :unsigned-byte
                                                    :internal-format :rgba8
                                                    :width resolution
                                                    :height resolution
                                                    :depth levels
                                                    :storage :static)))

(defmethod show-level ((clipmap geometry-clipmap) wcx wcy level)
  (let* ((cache (aref (mmap-cache clipmap) level))
         (scale (expt 2 level))
         (ts (resolution clipmap))
         (ws (* ts scale))
         (wx (- wcx (/ ws 2)))
         (wy (- wcy (/ ws 2)))
         (wl (* ws (floor wx ws)))
         (wu (* ws (floor wy ws)))
         (tx (floor wx scale))
         (ty (floor wy scale))
         (tl (floor wl scale))
         (tu (floor wu scale))
         (sx (- tx tl))
         (sy (- ty tu))
         (sw (- ts sx))
         (sh (- ts sy))
         (new-cache ())
         (dir (data-directory clipmap)))
    (labels ((path (bank wx wy)
               (format NIL "~a/~d/~a ~d ~d.raw" dir ws bank wx wy))
             (picture (tex file sx sy x y w h bpp)
               (when (and (< 0 w) (< 0 h))
                 (handler-case
                     (let ((cached (or (assoc file cache :test #'equal)
                                       (list* file (multiple-value-list (mmap:mmap file))))))
                       (push cached new-cache)
                       ;; Pretty dumb.
                       (when (and (= 0 level) (= 2 bpp)
                                  (<= (/ ts 2) w) (<= (/ ts 2) h))
                         (setf (current-height clipmap)
                               (* (cffi:mem-aref (second cached) :uint16
                                                 (+ (* ts (+ (/ ts 2) (if (= 0 y) sy (- y))))
                                                    (+ (/ ts 2) (if (= 0 x) sx (- x)))))
                                  (/ (expt 2.0 16))
                                  (vy (map-scale clipmap)))))
                       (%gl:tex-sub-image-3d :texture-2d-array 0 x y level w h 1 (pixel-format tex) (pixel-type tex)
                                             (cffi:inc-pointer (second cached) (* (+ (* ts sy) sx) bpp))))
                   (mmap:mmap-error (e)
                     (declare (ignore e))))))
             (show-map (bank tex bpp)
               (gl:bind-texture :texture-2d-array (gl-name tex))
               (picture tex (path bank (+ wl  0) (+ wu  0)) sx sy   0  0  sw sh  bpp)
               (picture tex (path bank (+ wl ws) (+ wu  0))  0 sy  sw  0  sx sh  bpp)
               (picture tex (path bank (+ wl  0) (+ wu ws)) sx  0   0 sh  sw sy  bpp)
               (picture tex (path bank (+ wl ws) (+ wu ws))  0  0  sw sh  sx sy  bpp)))
      ;; Update the texture buffer
      (gl:pixel-store :unpack-row-length ts)
      (show-map "height" (height-map clipmap) 2)
      (show-map "splat" (splat-map clipmap) 4)
      (gl:pixel-store :unpack-row-length 0)
      ;; Update mmap cache
      (let ((to-unmap (set-difference cache new-cache)))
        (loop for cached in to-unmap do (apply #'mmap:munmap (rest cached))))
      (setf (aref (mmap-cache clipmap) level) new-cache))))

(defmethod maybe-update-region ((clipmap geometry-clipmap))
  (let ((prev (previous-update-location clipmap))
        (x (vx (location clipmap)))
        (y (vz (location clipmap)))
        (s 1))
    (dotimes (l (levels clipmap) clipmap)
      (with-simple-restart (continue "Ignore level ~d." l)
        (when (or (/= (floor x s) (floor (vx prev) s))
                  (/= (floor y s) (floor (vy prev) s)))
          (show-level clipmap x y l)))
      (setf s (* s 2)))
    (setf (vx prev) x)
    (setf (vy prev) y)))

(defmethod show-region ((clipmap geometry-clipmap) x y)
  (setf (vx (location clipmap)) x)
  (setf (vz (location clipmap)) y)
  (maybe-update-region clipmap))

(defmethod paint ((clipmap geometry-clipmap) (pass shader-pass))
  (maybe-update-region clipmap)
  (let ((program (shader-program-for-pass pass clipmap))
        (levels (levels clipmap))
        (block (clipmap-block clipmap)))
    ;; (gl:polygon-mode :front-and-back :fill)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d-array (gl-name (height-map clipmap)))
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d-array (gl-name (splat-map clipmap)))
    (gl:bind-vertex-array (gl-name block))
    (setf (uniform program "height_map") 0)
    (setf (uniform program "splat_map") 1)
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "world_pos") (location clipmap))
    (setf (uniform program "levels") levels)
    (setf (uniform program "map_scale") (map-scale clipmap))
    (%gl:draw-elements-instanced :triangles (size block) :unsigned-int 0
                                 (+ 4 (* 12 levels)))))

(define-class-shader (geometry-clipmap :vertex-shader)
  "
// These factors are in [0,1], denoting the range between the
// outer ring (0) and the inner ring (1).
#define BORDER_OFFSET 0.1
#define BORDER_WIDTH 0.25

layout (location = 0) in vec3 position;
layout (location = 1) in float level;
layout (location = 2) in vec2 offset;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray height_map;
uniform int levels;
uniform vec3 world_pos;
uniform vec3 map_scale = vec3(1,1,1);

out CLIPMAP_DATA{
  float a;
  vec3 world;
  vec3 normal;
  vec3 tex_i;
  vec3 tex_o;
} clipmap_out;

void main(){
  float level_scale = pow(2.0, level);
  float n = textureSize(height_map, 0).x;
  vec2 map_pos = (position.xz + offset)/4;
  
  float a = 0;
  // Current level texture fetch
  vec2 tex_off = ((map_pos+0.5)-1/(n+1))*n;
  vec2 mov_off = mod(world_pos.xz, level_scale)/n;
  float y = texelFetch(height_map, ivec3(tex_off, level), 0).r;
  float yu = texelFetch(height_map, ivec3(min(n-1, tex_off.x+1), tex_off.y, level), 0).r;
  float yv = texelFetch(height_map, ivec3(tex_off.x, min(n-1, tex_off.y+1), level), 0).r;
  clipmap_out.tex_i = vec3(tex_off/n, level);
  clipmap_out.tex_o = clipmap_out.tex_i;
  
  if(level+1 < levels){
    // Outer level texture read
    vec2 tex_off_o = (map_pos/2+0.5)-0.5/n;
    vec2 mov_off_o = mod(world_pos.xz, level_scale*2)/n;
    float y_o = texture(height_map, vec3(tex_off_o, level+1)).r;
    float yu_o = texture(height_map, vec3(tex_off_o+vec2(1/n,0), level+1)).r;
    float yv_o = texture(height_map, vec3(tex_off_o+vec2(0,1/n), level+1)).r;
    
    // Inter-level blending factor
    vec2 alpha = (abs(map_pos)*2-0.5)*2;
    alpha = clamp((alpha+BORDER_OFFSET-(1-BORDER_WIDTH))/BORDER_WIDTH, 0, 1);
    a = max(alpha.x, alpha.y);
    
    // This is ALMOST perfect. There's a slight problem that's only really well
    // visible at low resolutions where off by 2 factors between regions cause the
    // blended region to pop. I have experimented for a while and haven't found a
    // perfect formula to remedy that yet, unfortunately.
    clipmap_out.tex_o = vec3(tex_off_o-a/(2*n), level+1);
    
    // Interpolate final Y
    mov_off = mix(mov_off, mov_off_o, a);
    y = mix(y, y_o, a);
    yu = mix(yu, yu_o, a);
    yv = mix(yv, yv_o, a);
  }
  
  vec2 world_2d = (map_pos * level_scale) - mov_off;

  clipmap_out.a = a;
  clipmap_out.world = world_pos + vec3(world_2d.x, y, world_2d.y)*map_scale;
  clipmap_out.normal = normalize(vec3(y-yu, 0.5, y-yv));

  gl_Position =  projection_matrix * view_matrix * vec4(clipmap_out.world, 1);
}")

(define-class-shader (geometry-clipmap :fragment-shader)
  "
uniform sampler2DArray splat_map;

in CLIPMAP_DATA{
  float a;
  vec3 world;
  vec3 normal;
  vec3 tex_i;
  vec3 tex_o;
} clipmap;
out vec4 color;

void main(){
  vec3 light = vec3(0.8, 0.45, 0.9);
  vec3 light_dir = normalize(light);
  float diff = max(dot(clipmap.normal, light_dir)*10-2.7, 0.0);
  vec4 splat = mix(texture(splat_map, clipmap.tex_i), texture(splat_map, clipmap.tex_o), clipmap.a);
  vec3 diffuse = (splat.g*vec3(1,1,1)+(1-splat.g)*vec3(0.239,0.275,0.191)*0.7) * diff;
  color = vec4(diffuse, 1.0);
}")

(defun make-clipmap-block (n levels)
  (let* ((m (/ n 4))
         (s (/ 4 n))
         (vao (change-class (make-quad-grid s m m) 'vertex-array))
         (array (make-array (* 3 (+ 4 (* 12 levels))) :element-type 'single-float))
         (vbo (make-instance 'vertex-buffer :buffer-data array))
         (i -1))
    (flet ((entry (i l x y)
             (setf (aref array (+ (* i 3) 0)) (float l 0f0))
             (setf (aref array (+ (* i 3) 1)) x)
             (setf (aref array (+ (* i 3) 2)) y)))
      (entry (incf i) 0 +0.5 +0.5)
      (entry (incf i) 0 +0.5 -0.5)
      (entry (incf i) 0 -0.5 -0.5)
      (entry (incf i) 0 -0.5 +0.5)
      (loop for l from 0 below levels
            do (entry (incf i) l +1.5 +1.5)
               (entry (incf i) l +0.5 +1.5)
               (entry (incf i) l -0.5 +1.5)
               (entry (incf i) l -1.5 +1.5)
               (entry (incf i) l -1.5 +0.5)
               (entry (incf i) l -1.5 -0.5)
               (entry (incf i) l -1.5 -1.5)
               (entry (incf i) l -0.5 -1.5)
               (entry (incf i) l +0.5 -1.5)
               (entry (incf i) l +1.5 -1.5)
               (entry (incf i) l +1.5 -0.5)
               (entry (incf i) l +1.5 +0.5)))
    (push (list vbo :index 1 :offset 0 :size 1 :stride (* 3 4) :instancing 1) (bindings vao))
    (push (list vbo :index 2 :offset 4 :size 2 :stride (* 3 4) :instancing 1) (bindings vao))
    vao))

(defun sub-image (pixels ow c x y w h &optional out-pixels)
  (let ((out-pixels (or out-pixels (make-array (* w h c)
                                               :element-type (array-element-type pixels)))))
    (loop for i from 0 below h
          do (loop for j from 0 below w
                   for oi = (* (+ (* i w) j) c)
                   for ii = (* (+ (* (+ i y) ow) j x) c)
                   do (loop for k from 0 below c
                            do (setf (aref out-pixels (+ oi k)) (aref pixels (+ ii k))))))
    out-pixels))

(defun halve-image (pixels ow oh c &optional out-pixels)
  (let* ((w (/ ow 2))
         (h (/ oh 2))
         (out-pixels (or out-pixels (make-array (* w h c)
                                                :element-type (array-element-type pixels))))
         (fit (cond ((eq (array-element-type pixels) 'single-float)
                     (lambda (a) (coerce a 'single-float)))
                    ((eq (array-element-type pixels) 'double-float)
                     (lambda (a) (coerce a 'double-float)))
                    (T
                     (lambda (a) (round a))))))
    (loop for i from 0 below h
          do (loop for j from 0 below w
                   for oi = (* (+ (* i w) j) c)
                   for ii = (* (+ (* i 2 ow) (* j 2)) c)
                   do (loop for k from 0 below c
                            for p1 = (aref pixels (+ k ii))
                            for p2 = (aref pixels (+ k ii c))
                            for p3 = (aref pixels (+ k ii (* ow c)))
                            for p4 = (aref pixels (+ k ii (* ow c) c))
                            do (setf (aref out-pixels (+ oi k)) (funcall fit (/ (+ p1 p2 p3 p4) 4))))))
    out-pixels))

(defun generate-clipmaps (input output &key (n *default-clipmap-resolution*) (levels 5) ((:x xoff) 0) ((:y yoff) 0)
                                            depth pixel-type (format :red) (bank "height"))
  (format T "~& Reading image data ~a ...~%" input)
  (multiple-value-bind (bits w h depth type format) (load-image input T :depth depth
                                                                        :pixel-type pixel-type
                                                                        :format format)
    ;; FIXME: remove this constraint
    (assert (eql w h))
    (let* ((w (or w (sqrt (length bits))))
           (h (or h w))
           (c (format-components format))
           (sub (make-array (* n n c) :element-type (array-element-type bits)))
           (bits bits))
      (flet ((clipmap (o x y s)
               (sub-image bits (/ w s) c (/ x s) (/ y s) n n sub)
               (with-open-file (out o :direction :output
                                      :if-exists :supersede
                                      :element-type (array-element-type sub))
                 (write-sequence sub out))))
        (dotimes (l levels output)
          (let* ((s (expt 2 l))
                 (o (pathname-utils:subdirectory output (princ-to-string (* n s)))))
            (format T "~& Generating level ~d (~d tile~:p)...~%" l (expt (/ w s n) 2))
            (ensure-directories-exist o)
            (loop for x from 0 below w by (* n s)
                  do (loop for y from 0 below h by (* n s)
                           for f = (make-pathname :name (format NIL "~a ~d ~d"
                                                                bank
                                                                (- (+ x xoff) (/ w 2))
                                                                (- (+ y yoff) (/ h 2)))
                                                  :type "raw"
                                                  :defaults o)
                           do (clipmap f x y s)))
            ;; Shrink by a factor of 2.
            (setf bits (halve-image bits (/ w s) (/ h s) c))))))
    (free-image-data bits)))
