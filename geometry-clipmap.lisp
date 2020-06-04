#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *default-clipmap-resolution* 128)

(defstruct (geometry-clipmap-map
            (:constructor make-clipmap-map (index bank uniform texture)))
  (index 0 :type (unsigned-byte 8))
  (bank NIL :type string)
  (uniform NIL :type string)
  (texture NIL :type texture))

(defmethod compute-resources ((clipmap geometry-clipmap-map) resources readying cache)
  (vector-push-extend (geometry-clipmap-map-texture clipmap) resources))

(define-shader-entity geometry-clipmap (located-entity)
  ((previous-update-location :initform (vec2 most-positive-single-float most-positive-single-float)
                             :accessor previous-update-location)
   (current-height :accessor current-height)
   (mmap-cache :accessor mmap-cache)
   (clipmap-block :accessor clipmap-block)
   (levels :accessor levels)
   (resolution :accessor resolution)
   (map-scale :initarg :map-scale :accessor map-scale)
   (maps :initform () :accessor maps)
   (data-directory :initarg :data-directory :accessor data-directory))
  (:default-initargs
   :map-scale (vec 1 1 1)
   :maps '("height")
   :data-directory (error "DATA-DIRECTORY required.")))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key maps data-directory)
  (loop for i from 0
        for map in maps
        do (destructuring-bind (bank &key (index i) (uniform (format NIL "~a_map" bank)))
               (enlist map)
             (with-open-file (stream (make-pathname :name bank :type "lisp" :defaults data-directory))
               (push (make-clipmap-map index bank uniform
                                       (apply #'make-instance 'texture
                                              :target :texture-2d-array
                                              :min-filter :linear
                                              :storage :static
                                              (read stream)))
                     (maps clipmap)))))
  (setf (maps clipmap) (sort (maps clipmap) #'< :key #'geometry-clipmap-map-index))
  (setf (data-directory clipmap) (uiop:native-namestring (make-pathname :name NIL :type NIL :defaults data-directory)))
  (let ((base-texture (geometry-clipmap-map-texture (first (maps clipmap)))))
    (setf (resolution clipmap) (width base-texture))
    (setf (levels clipmap) (depth base-texture)))
  (setf (mmap-cache clipmap) (make-array (levels clipmap) :initial-element ()))
  (setf (clipmap-block clipmap) (make-clipmap-block (resolution clipmap) (levels clipmap))))

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
             (picture (bank tex file sx sy x y w h)
               (when (and (< 0 w) (< 0 h))
                 (handler-case
                     (let ((cached (or (assoc file cache :test #'equal)
                                       (list* file (multiple-value-list (mmap:mmap file))))))
                       (push cached new-cache)
                       ;; FIXME: Calculate height and slope in a non-retarded way.
                       (when (and (= 0 level) (<= (/ ts 2) w) (<= (/ ts 2) h)
                                  (string= bank "height"))
                         (setf (current-height clipmap)
                               (* (cffi:mem-aref (second cached) :uint16
                                                 (+ (* ts (+ (/ ts 2) (if (= 0 y) sy (- y))))
                                                    (+ (/ ts 2) (if (= 0 x) sx (- x)))))
                                  (/ (expt 2.0 16))
                                  (vy (map-scale clipmap)))))
                       (%gl:tex-sub-image-3d :texture-2d-array 0 x y level w h 1 (pixel-format tex) (pixel-type tex)
                                             (cffi:inc-pointer (second cached) (* (+ (* ts sy) sx)
                                                                                  (/ (internal-format-pixel-size
                                                                                      (internal-format tex))
                                                                                     8)))))
                   (mmap:mmap-error (e)
                     (declare (ignore e))))))
             (show-map (bank tex)
               (gl:bind-texture :texture-2d-array (gl-name tex))
               (picture bank tex (path bank (+ wl  0) (+ wu  0)) sx sy   0  0  sw sh)
               (picture bank tex (path bank (+ wl ws) (+ wu  0))  0 sy  sw  0  sx sh)
               (picture bank tex (path bank (+ wl  0) (+ wu ws)) sx  0   0 sh  sw sy)
               (picture bank tex (path bank (+ wl ws) (+ wu ws))  0  0  sw sh  sx sy)))
      ;; Update the texture buffer
      (gl:pixel-store :unpack-row-length ts)
      ;; FIXME: What about different resolution banks?
      (dolist (map (maps clipmap))
        (show-map (geometry-clipmap-map-bank map) (geometry-clipmap-map-texture map)))
      (gl:pixel-store :unpack-row-length 0)
      ;; Update mmap cache
      (let ((to-unmap (set-difference cache new-cache)))
        (loop for cached in to-unmap do (apply #'mmap:munmap (rest cached))))
      (setf (aref (mmap-cache clipmap) level) new-cache))))

(defmethod maybe-update-region ((clipmap geometry-clipmap))
  (let ((prev (previous-update-location clipmap))
        (x (vx (location clipmap)))
        (y (vz (location clipmap)))
        (s 1.0))
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

(defmethod render ((clipmap geometry-clipmap) (program shader-program))
  (maybe-update-region clipmap)
  (let ((levels (levels clipmap))
        (block (clipmap-block clipmap)))
    ;;(gl:polygon-mode :front-and-back :fill)
    ;;(gl:polygon-mode :front-and-back :line)
    (dolist (map (maps clipmap))
      (gl:active-texture (+ (load-time-value (cffi:foreign-enum-value '%gl:enum :texture0))
                            (geometry-clipmap-map-index map)))
      (gl:bind-texture :texture-2d-array (gl-name (geometry-clipmap-map-texture map)))
      (setf (uniform program (geometry-clipmap-map-uniform map)) (geometry-clipmap-map-index map)))
    (gl:bind-vertex-array (gl-name block))
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
  vec3 fog = vec3(1,1,1);
  vec3 sky = vec3(112.0/255,175.0/255,224.0/255);
  vec3 light = vec3(0.8, 0.45, 0.9);
  vec3 light_dir = normalize(light);
  float diff = max(dot(clipmap.normal, light_dir)*10-2.7, 0.0);
  vec4 splat = mix(texture(splat_map, clipmap.tex_i), texture(splat_map, clipmap.tex_o), clipmap.a);
  vec3 diffuse = (splat.g*vec3(1,1,1)+(1-splat.g)*vec3(0.30,0.27,0.27)*0.7) * diff;
  float d = clamp(pow(1.0/((1-gl_FragCoord.z)*8000), 2), 0, 1);
  color = vec4(mix(diffuse, sky, d), 1);
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

(defun generate-clipmaps (input output &key (resolution *default-clipmap-resolution*) (levels 5) ((:x xoff) 0) ((:y yoff) 0)
                                            pixel-type (pixel-format :red) (bank "height"))
  (format T "~&Reading image data ~a ...~%" input)
  (multiple-value-bind (bits w h pixel-type pixel-format)
      (load-image input T :pixel-type pixel-type :pixel-format pixel-format)
    (format T "~&Image is ~dx~d ~a ~a~%" w h pixel-type pixel-format)
    ;; FIXME: remove this constraint
    (assert (eql w h))
    (with-open-file (out (make-pathname :name bank :type "lisp" :defaults output)
                         :direction :output
                         :if-exists :supersede)
      (format out "(~@{~(~s~) ~s~^~% ~})"
              :internal-format (infer-internal-format pixel-type pixel-format)
              :pixel-format pixel-format
              :pixel-type pixel-type
              :width resolution
              :height resolution
              :depth levels))
    (let* ((w (or w (sqrt (length bits))))
           (h (or h w))
           (c (internal-format-components pixel-format))
           (n resolution)
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
            (format T "~&Generating level ~d (~d tile~:p)...~%" l (expt (/ w s n) 2))
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
    (free-data bits)))
