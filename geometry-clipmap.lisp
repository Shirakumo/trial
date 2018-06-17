#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity geometry-clipmap ()
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

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key resolution levels)
  (setf (clipmap-block clipmap) (make-clipmap-block resolution))
  (setf (maps clipmap) (make-instance 'texture :target :texture-2d-array
                                               :min-filter :linear
                                               :width resolution
                                               :height resolution
                                               :depth levels))
  (setf (texture-buffer clipmap) (make-instance 'texture :target :texture-2d
                                                         :width (* 2 resolution)
                                                         :height (* 2 resolution))))

(defmethod show-level ((clipmap geometry-clipmap) x y level)
  (let* ((r (resolution clipmap))
         (s (* r (expt 2 level)))
         (x (- x (/ s 2)))
         (y (- y (/ s 2)))
         (l (* s (floor x s)))
         (u (* s (floor y s)))
         (dir (data-directory clipmap)))
    ;; Update the texture buffer
    (flet ((picture (file x y)
             (let ((data (cl-soil:load-image (if (probe-file file) file #p"~/clipmaps/zero.png"))))
               (unwind-protect
                    (%gl:tex-sub-image-2d :texture-2d 0 x y r r :rgba :unsigned-byte data)
                 (cl-soil:free-image-data data))))
           (path (x y)
             (merge-pathnames (make-pathname :name (format NIL "~d,~d" x y) :type "png"
                                             :directory `(:relative ,(princ-to-string s)))
                              dir)))
      (picture (path (+ l 0) (+ u 0)) 0 0)
      (picture (path (+ l s) (+ u 0)) r 0)
      (picture (path (+ l s) (+ u s)) r r)
      (picture (path (+ l 0) (+ u s)) 0 r))
    ;; Draw into the clipmap layer
    (%gl:copy-image-sub-data (gl-name (texture-buffer clipmap)) :texture-2d 0 (/ (- x l) (expt 2 level)) (/ (- y u) (expt 2 level)) 0
                             (gl-name (maps clipmap)) :texture-2d-array 0 0 0 level r r 1)))

(defmethod show-region ((clipmap geometry-clipmap) x y)
  ;; FIXME: Remember what levels show and only redraw ones
  ;;        that need updating.
  (gl:bind-texture :texture-2d (gl-name (texture-buffer clipmap)))
  (dotimes (l (levels clipmap) clipmap)
    (with-simple-restart (continue "Ignore level ~d." l)
      (show-level clipmap x y l))))

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

out float z;

void main(){
  float n = textureSize(texture_image, 0).x;
  vec2 map_pos = position.xz + offset;
  vec2 tex_off = (map_pos/4+0.5);

  z = texelFetch(texture_image, ivec3(tex_off*n, level), 0).r;
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
  gl_Position = projection_matrix * view_matrix * vec4(world.x, z*200, world.y, 1.0);
}")

(define-class-shader (geometry-clipmap :fragment-shader)
  "
in float z;
out vec4 color;

void main(){
  color = vec4(z, z, z, 1);
}")

(defun make-clipmap-block (n)
  (let ((m (/ n 4))
        (s (/ 4 n)))
    (change-class (make-quad-grid s m m) 'vertex-array)))

;; Files generated as X,Y.png where X and Y are offset from the center of the map
;; in full-scale pixel coordinates. This means that the pixel sizes are assumed to
;; correspond to in-game unit sizes. If not, scaling must be applied to account
;; for the difference.
(defun generate-clipmaps (input output &key (n 64) (levels 5) ((:x xoff) 0) ((:y yoff) 0))
  (let ((total (loop for l from 0 below levels
                     sum (expt (expt 2 l) 2)))
        (counter 0) (printed 0))
    (flet ((clipmap (o x y s)
             (uiop:run-program (list "magick" "convert"
                                     (uiop:native-namestring input)
                                     "-gravity" "center"
                                     "-crop" (format NIL "~dx~:*~d+~d+~d!" s x y)
                                     "-background" "black"
                                     "-flatten"
                                     "-scale" (format NIL "~dx~d!" n n)
                                     (uiop:native-namestring o))
                               :error-output T)
             (incf counter)
             (let ((percentage (round (/ counter total 0.01))))
               (when (and (/= percentage printed) (= 0 (mod percentage 10)))
                 (setf printed percentage)
                 (format T "~& ~2d%~%" percentage)))))
      (multiple-value-bind (d w h) (cl-soil:load-image input)
        (cl-soil:free-image-data d)
        (dotimes (l levels output)
          (let* ((s (* n (expt 2 l)))
                 (o (pathname-utils:subdirectory output (princ-to-string s))))
            (ensure-directories-exist o)
            (loop for x from (/ w -2) below (/ w 2) by s
                  do (loop for y from (/ h -2) below (/ h 2) by s
                           for f = (make-pathname :name (format NIL "~d,~d" (+ x xoff) (+ y yoff)) :type "png" :defaults o)
                           do (clipmap f (+ x (/ s 2)) (+ y (/ s 2)) s))))))
      (values counter total))))
