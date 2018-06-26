#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity geometry-clipmap (located-entity)
  ((clipmap-block :accessor clipmap-block)
   (levels :initarg :levels :accessor levels)
   (resolution :initarg :resolution :accessor resolution)
   (maps :accessor maps)
   (texture-buffer :accessor texture-buffer)
   (data-directory :initarg :data-directory :accessor data-directory))
  (:default-initargs
   :levels 5
   :resolution 64
   :data-directory (error "DATA-DIRECTORY required.")))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key levels data-directory resolution)
  (setf (clipmap-block clipmap) (make-clipmap-block resolution))
  (setf (maps clipmap) (make-instance 'texture :target :texture-2d-array
                                               :min-filter :linear
                                               :pixel-format pixel-format
                                               :pixel-type (infer-pixel-type bittage)
                                               :internal-format (infer-internal-format bittage pixel-format)
                                               :width resolution
                                               :height resolution
                                               :depth levels))
  (setf (texture-buffer clipmap) (make-instance 'texture :target :texture-2d
                                                         :min-filter :linear
                                                         :pixel-format pixel-format
                                                         :pixel-type (infer-pixel-type bittage)
                                                         :internal-format (infer-internal-format bittage pixel-format)
                                                         :width (* 2 resolution)
                                                         :height (* 2 resolution))))

(defmethod show-level ((clipmap geometry-clipmap) x y level)
  (let* ((r (resolution clipmap))
         (s (* r (expt 2 level)))
         (x (- x (/ s 2)))
         (y (- y (/ s 2)))
         (l (* s (floor x s)))
         (u (* s (floor y s)))
         (dir (data-directory clipmap))
         (tex (texture-buffer clipmap)))
    ;; Update the texture buffer
    (flet ((picture (file x y)
             (if (probe-file file)
                 (let ((data (load-image file T)))
                   (unwind-protect
                        (%gl:tex-sub-image-2d :texture-2d 0 x y r r (pixel-format tex) (pixel-type tex) (coerce-pixel-data data))
                     (free-image-data data)))
                 ;; FIXME: Requires GL 4.4
                 (cffi:with-foreign-object (data :uint64 4)
                   (dotimes (i 4) (setf (cffi:mem-aref data :uint64 i) 0))
                   (%gl:clear-tex-sub-image (gl-name tex) 0 x y 0 r r 1 (pixel-format tex) (pixel-type tex) data))))
           (path (x y)
             (merge-pathnames (make-pathname :name (format NIL "~d,~d" x y) :type "png"
                                             :directory `(:relative ,(princ-to-string s)))
                              dir)))
      (picture (path (+ l 0) (+ u 0)) 0 r)
      (picture (path (+ l s) (+ u 0)) r r)
      (picture (path (+ l s) (+ u s)) r 0)
      (picture (path (+ l 0) (+ u s)) 0 0))
    ;; FIXME: Requires GL 4.3
    (%gl:copy-image-sub-data (gl-name tex) :texture-2d 0 (/ (- x l) (expt 2 level)) (/ (- y u) (expt 2 level)) 0
                             (gl-name (maps clipmap)) :texture-2d-array 0 0 0 level r r 1)))

(defmethod show-region ((clipmap geometry-clipmap) x y)
  (gl:bind-texture :texture-2d (gl-name (texture-buffer clipmap)))
  (dotimes (l (levels clipmap) clipmap)
    (with-simple-restart (continue "Ignore level ~d." l)
      (show-level clipmap x y l)))
  (setf (vx (location clipmap)) x)
  (setf (vy (location clipmap)) y))

(defmethod maybe-show-region ((clipmap geometry-clipmap) x y)
  (gl:bind-texture :texture-2d (gl-name (texture-buffer clipmap)))
  (let ((old (location clipmap))
        (s (resolution clipmap)))
    (dotimes (l (levels clipmap) clipmap)
      (with-simple-restart (continue "Ignore level ~d." l)
        (when (or (/= (floor x s) (floor (vx old) s))
                  (/= (floor y s) (floor (vy old) s)))
          (show-level clipmap x y l)))
      (setf s (* s 2)))))

(defmethod (setf location) :before (new (clipmap geometry-clipmap))
  ;; FIXME: Weird semantics, not sure how to reconcile.
  (if (eql new (location clipmap))
      (maybe-show-region clipmap (vx new) (vy new))
      (show-region clipmap (vx new) (vy new))))

(defmethod paint ((clipmap geometry-clipmap) (pass shader-pass))
  (let ((program (shader-program-for-pass pass clipmap))
        (levels (levels clipmap))
        (maps (maps clipmap))
        (clipmap (clipmap-block clipmap)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d-array (gl-name maps))
    (gl:bind-vertex-array (gl-name clipmap))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix))
    (setf (uniform program "level") 0)
    (setf (uniform program "scale") 16.0)
    (setf (uniform program "levels") levels)
    (flet ((paint (x z)
             (setf (uniform program "offset") (vec x z))
             (%gl:draw-elements :triangles (size clipmap) :unsigned-int 0)))
      (paint +0.5 +0.5)
      (paint +0.5 -0.5)
      (paint -0.5 -0.5)
      (paint -0.5 +0.5)
      (loop for scale = 16.0s0 then (* scale 2.0s0)
            for level from 0 below levels
            do (setf (uniform program "level") level)
               (setf (uniform program "scale") scale)
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
#define BORDER 10.0

layout (location = 0) in vec3 position;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray texture_image;
uniform int levels;
uniform int level;
uniform float scale;
uniform vec2 offset;

out vec4 pos;

void main(){
  float n = textureSize(texture_image, 0).x;
  vec2 map_pos = position.xz + offset;
  vec2 tex_off = (map_pos/4+0.5);

  float z = texelFetch(texture_image, ivec3(tex_off*n, level), 0).r;
  if(level+1 < levels){
    // Inter-level blending factor
    vec2 alpha = clamp((abs(map_pos)-2)*BORDER+1, 0, 1);
    float a = max(alpha.x, alpha.y);
  
    // Retrieve outer Z factor by interpolated texel read.
    vec2 tex_off_i = (map_pos/8+0.5)+0.5/n;
    float zo = texture(texture_image, vec3(tex_off_i, level+1)).r;

    // Interpolate final Z
    z = mix(z, zo, a);
  }

  vec2 world = map_pos * scale;
  pos = vec4(world.x, z*200, world.y, 1.0);
  gl_Position =  projection_matrix * view_matrix * pos;
}")

(define-class-shader (geometry-clipmap :fragment-shader)
  "
void main(){
  
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

(defun generate-clipmaps (input output &key (n 64) (levels 5) ((:x xoff) 0) ((:y yoff) 0)
                                            depth pixel-type (format :red) (bank "height"))
  (multiple-value-bind (bits w h depth type format) (load-image input T :depth depth
                                                                        :pixel-type pixel-type
                                                                        :format format)
    (declare (ignore depth type))
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
                           for f = (make-pathname :name (format NIL "~a-~d,~d"
                                                                bank
                                                                (- (+ x xoff) (/ w 2))
                                                                (- (+ y yoff) (/ h 2)))
                                                  :type "raw"
                                                  :defaults o)
                           do (clipmap f x y s)))
            ;; Shrink by a factor of 2.
            (setf bits (halve-image bits (/ w s) (/ h s) c))))))
    (free-image-data bits)))
