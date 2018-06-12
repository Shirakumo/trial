#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity geometry-clipmap ()
  ((clipmap-block :accessor clipmap-block)
   (levels :initarg :levels :accessor levels)
   (texture :initarg :texture :accessor texture))
  (:default-initargs
   :levels 5))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key (n 64))
  (setf (clipmap-block clipmap) (make-clipmap-block n)))

(defmethod paint ((clipmap geometry-clipmap) (pass shader-pass))
  (gl:polygon-mode :front-and-back :fill)
  (let ((program (shader-program-for-pass pass clipmap))
        (levels (levels clipmap))
        (texture (texture clipmap))
        (clipmap (clipmap-block clipmap)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d-array (gl-name texture))
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
  "layout (location = 0) in vec3 position;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray texture_image;
uniform int levels;
uniform int level;
uniform float scale;
uniform vec2 offset;

out float z;

void main(){
  float border = 10;
  float n = textureSize(texture_image, 0).x;
  vec2 map_pos = position.xz + offset;
  vec2 tex_off = (map_pos/4+0.5);

  z = texelFetch(texture_image, ivec3(tex_off*n, level), 0).r;
  if(level+1 < levels){
    // Inter-level blending factor
    vec2 alpha = clamp(abs(map_pos)*border-(border*2)-1, 0, 1);
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

(defun generate-clipmaps (input output &key (n 64) (levels 5))
  (let ((total (loop for l from 0 below levels
                     sum (expt (1+ (expt 2 l)) 2)))
        (counter 0) (printed 0)
        (levels (1- levels)))
    (flet ((clipmap (o x y s)
             (let ((o (make-pathname :name (format NIL "~d,~d" x y) :type "png" :defaults o)))
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
                   (format T "~& ~2d%~%" percentage))))))
      (multiple-value-bind (d w h) (cl-soil:load-image input)
        (cl-soil:free-image-data d)
        (dotimes (l levels output)
          (let ((s (* n (expt 2 l)))
                (o (pathname-utils:subdirectory output (princ-to-string l))))
            (ensure-directories-exist o)
            (loop for x from (/ w -2) to (/ w 2) by s
                  do (loop for y from (/ h -2) to (/ h 2) by s
                           do (clipmap o x y s)))))
        (let ((o (pathname-utils:subdirectory output (princ-to-string levels))))
          (ensure-directories-exist o)
          (clipmap o 0 0 w)))
      (values counter total))))
