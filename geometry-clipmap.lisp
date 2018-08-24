#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *default-clipmap-resolution* 128)

(define-shader-entity geometry-clipmap (located-entity)
  ((previous-update-location :initform (vec2 0 0) :accessor previous-update-location)
   (clipmap-block :accessor clipmap-block)
   (levels :initarg :levels :accessor levels)
   (resolution :initarg :resolution :accessor resolution)
   (map-scale :initarg :map-scale :accessor map-scale)
   (height-map :accessor height-map)
   (height-buffer :accessor height-buffer)
   (splat-map :accessor splat-map)
   (splat-buffer :accessor splat-buffer)
   (data-directory :initarg :data-directory :accessor data-directory))
  (:default-initargs
   :levels 5
   :map-scale (vec 1 1 1)
   :resolution *default-clipmap-resolution*
   :data-directory (error "DATA-DIRECTORY required.")))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key levels resolution)
  (setf (clipmap-block clipmap) (make-clipmap-block resolution))
  (setf (height-map clipmap) (make-instance 'texture :target :texture-2d-array
                                                     :min-filter :linear
                                                     :internal-format :r16
                                                     :width resolution
                                                     :height resolution
                                                     :depth levels
                                                     :storage :static))
  (setf (height-buffer clipmap) (make-instance 'texture :target :texture-2d
                                                        :min-filter :linear
                                                        :pixel-format :red
                                                        :pixel-type :unsigned-short
                                                        :internal-format :r16
                                                        :width (* 2 resolution)
                                                        :height (* 2 resolution)
                                                        :storage :static))
  (setf (splat-map clipmap) (make-instance 'texture :target :texture-2d-array
                                                     :min-filter :linear
                                                     :internal-format :rgba8
                                                     :width resolution
                                                     :height resolution
                                                     :depth levels
                                                     :storage :static))
  (setf (splat-buffer clipmap) (make-instance 'texture :target :texture-2d
                                                       :min-filter :linear
                                                       :pixel-format :rgba
                                                       :pixel-type :unsigned-byte
                                                       :internal-format :rgba8
                                                       :width (* 2 resolution)
                                                       :height (* 2 resolution)
                                                       :storage :static)))

(defmethod show-level ((clipmap geometry-clipmap) x y level)
  (let* ((r (resolution clipmap))
         (s (* r (expt 2 level)))
         (x (- x (/ s 2)))
         (y (+ y (/ s 2)))
         (l (* s (floor x s)))
         (u (* s (floor y s)))
         (dir (append (pathname-directory (data-directory clipmap))
                      (list (princ-to-string s)))))
    ;; Update the texture buffer
    (labels ((picture (tex file x y)
               (cond ((probe-file file)
                      (mmap:with-mmap (ptr size file)
                        (%gl:tex-sub-image-2d :texture-2d 0 x y r r (pixel-format tex) (pixel-type tex) ptr)))
                     (T
                      ;; FIXME: Requires GL 4.4
                      (cffi:with-foreign-object (data :uint64 4)
                        (dotimes (i 4) (setf (cffi:mem-aref data :uint64 i) 0))
                        (%gl:clear-tex-sub-image (gl-name tex) 0 x y 0 r r 1 (pixel-format tex) (pixel-type tex) data)))))
             (path (bank x y)
               (make-pathname :name (format NIL "~a ~d ~d" bank x y) :type "raw" :directory dir))
             (show-map (bank tex target)
               (gl:bind-texture :texture-2d (gl-name tex))
               (picture tex (path bank (+ l 0) (+ u 0)) 0 r)
               (picture tex (path bank (+ l s) (+ u 0)) r r)
               (picture tex (path bank (+ l s) (- u s)) r 0)
               (picture tex (path bank (+ l 0) (- u s)) 0 0)
               ;; FIXME: Requires GL 4.3
               (%gl:copy-image-sub-data (gl-name tex) :texture-2d 0 (/ (- x l) (expt 2 level)) (/ (- y u) (expt 2 level)) 0
                                        (gl-name target) :texture-2d-array 0 0 0 level r r 1)))
      (show-map "height" (height-buffer clipmap) (height-map clipmap))
      (show-map "splat" (splat-buffer clipmap) (splat-map clipmap)))))

(defmethod show-region ((clipmap geometry-clipmap) x y)
  (dotimes (l (levels clipmap) clipmap)
    (with-simple-restart (continue "Ignore level ~d." l)
      (show-level clipmap x y l)))
  (setf (vx (previous-update-location clipmap)) x)
  (setf (vz (previous-update-location clipmap)) y))

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

(defmethod paint ((clipmap geometry-clipmap) (pass shader-pass))
  (maybe-update-region clipmap)
  (let ((program (shader-program-for-pass pass clipmap))
        (levels (levels clipmap))
        (block (clipmap-block clipmap)))
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
    (setf (uniform program "level") 0)
    (setf (uniform program "map_scale") (map-scale clipmap))
    (flet ((paint (x z)
             (setf (uniform program "offset") (vec x z))
             (%gl:draw-elements :triangles (size block) :unsigned-int 0)))
      (paint +0.5 +0.5)
      (paint +0.5 -0.5)
      (paint -0.5 -0.5)
      (paint -0.5 +0.5)
      (loop for level from 0 below levels
            do (setf (uniform program "level") level)
               ;; FIXME: use instancing for this
               (paint +1.5 +1.5)
               (paint +0.5 +1.5)
               (paint -0.5 +1.5)
               (paint -1.5 +1.5)
               (paint -1.5 +0.5)
               (paint -1.5 -0.5)
               (paint -1.5 -1.5)
               (paint -0.5 -1.5)
               (paint +0.5 -1.5)
               (paint +1.5 -1.5)
               (paint +1.5 -0.5)
               (paint +1.5 +0.5)))))

(define-class-shader (geometry-clipmap :vertex-shader)
  "
// Factor for the width of the blending border. Higher means smaller.
#define BORDER 2.0

layout (location = 0) in vec3 position;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray height_map;
uniform int levels;
uniform int level;
uniform vec2 offset;
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
  vec2 map_pos = position.xz + offset;
  
  float a = 0;
  // Current level texture fetch
  vec2 tex_off = ((map_pos/4+0.5)-1/(n+1))*n;
  vec2 mov_off = mod(world_pos.xz, level_scale)*(4/n);
  float y = texelFetch(height_map, ivec3(tex_off, level), 0).r;
  float yu = texelFetch(height_map, ivec3(min(n-1, tex_off.x+1), tex_off.y, level), 0).r;
  float yv = texelFetch(height_map, ivec3(tex_off.x, min(n-1, tex_off.y+1), level), 0).r;
  clipmap_out.tex_i = vec3(tex_off/n, level);
  clipmap_out.tex_o = clipmap_out.tex_i;
  
  if(level+1 < levels){
    // Outer level texture read
    vec2 tex_off_o = (map_pos/8+0.5)+0.5/n-1/(n+1);
    vec2 mov_off_o = mod(world_pos.xz, level_scale*2)*(4/n);
    float y_o = texture(height_map, vec3(tex_off_o, level+1)).r;
    float yu_o = texture(height_map, vec3(tex_off_o+vec2(1/n,0), level+1)).r;
    float yv_o = texture(height_map, vec3(tex_off_o+vec2(0,1/n), level+1)).r;
    clipmap_out.tex_o = vec3(tex_off_o, level+1);
    
    // Inter-level blending factor
    vec2 alpha = clamp((abs(map_pos)-2)*BORDER+1, 0, 1);
    a = max(alpha.x, alpha.y);
    
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

;; This shader ensures that zerod heights get a hole punched.
;; (define-class-shader (geometry-clipmap :geometry-shader)
;;   "#version 330 core
;; layout(triangles) in;
;; layout(triangle_strip, max_vertices = 3) out;
;; in CLIPMAP_DATA{
;;   float a;
;;   vec3 world;
;;   vec3 normal;
;;   vec3 tex_i;
;;   vec3 tex_o;
;; } clipmap_in[];
;; out CLIPMAP_DATA{
;;   float a;
;;   vec3 world;
;;   vec3 normal;
;;   vec3 tex_i;
;;   vec3 tex_o;
;; } clipmap_out;

;; void main(){
;;   if(clipmap_in[0].world.y*clipmap_in[1].world.y*clipmap_in[2].world.y != 0){
;;     int i;
;;     for(i = 0;i < gl_in.length();i++){
;;       clipmap_out.a = clipmap_in[i].a;
;;       clipmap_out.world = clipmap_in[i].world;
;;       clipmap_out.normal = clipmap_in[i].normal;
;;       clipmap_out.tex_i = clipmap_in[i].tex_i;
;;       clipmap_out.tex_o = clipmap_in[i].tex_o;
;;       gl_Position = gl_in[i].gl_Position;
;;       EmitVertex();
;;     }
;;     EndPrimitive();
;;   }
;; }")

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
  vec3 light = vec3(1, 0.5, 0.8);
  vec3 light_dir = normalize(light);
  float diff = max(dot(clipmap.normal, light_dir)*10-2.7, 0.0);
  vec4 splat = mix(texture(splat_map, clipmap.tex_i), texture(splat_map, clipmap.tex_o), clipmap.a);
  vec3 diffuse = (splat.g*vec3(1,1,1)+(1-splat.g)*vec3(0.239,0.275,0.191)*0.7) * diff;
  color = vec4(diffuse, 1.0);
}")

(defun make-clipmap-block (n)
  (let ((m (/ n 4))
        (s (/ 4 n)))
    (change-class (make-quad-grid s m m) 'vertex-array)))

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
  (multiple-value-bind (bits w h depth type format) (load-image input T :depth depth
                                                                        :pixel-type pixel-type
                                                                        :format format)
    
    (print (list w h depth type format (array-element-type bits)))
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
