#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity geometry-clipmap (located-entity)
  ((clipmap-block :accessor clipmap-block)
   (levels :initarg :levels :accessor levels)
   (resolution :accessor resolution)
   (maps :accessor maps)
   (texture-buffer :accessor texture-buffer)
   (data-directory :initarg :data-directory :accessor data-directory))
  (:default-initargs
   :levels 5
   :data-directory (error "DATA-DIRECTORY required.")))

(defmethod initialize-instance :after ((clipmap geometry-clipmap) &key levels data-directory)
  (multiple-value-bind (data width height bittage pixel-format) (load-image (make-pathname :name "zero" :type "png" :defaults data-directory) T)
    (assert (= width height))
    (free-image-data data)
    (setf (resolution clipmap) width)
    (setf (clipmap-block clipmap) (make-clipmap-block width))
    (setf (maps clipmap) (make-instance 'texture :target :texture-2d-array
                                                 :min-filter :linear
                                                 :pixel-format pixel-format
                                                 :pixel-type (infer-pixel-type bittage)
                                                 :internal-format (infer-internal-format bittage pixel-format)
                                                 :width width
                                                 :height width
                                                 :depth levels))
    (setf (texture-buffer clipmap) (make-instance 'texture :target :texture-2d
                                                           :min-filter :linear
                                                           :pixel-format pixel-format
                                                           :pixel-type (infer-pixel-type bittage)
                                                           :internal-format (infer-internal-format bittage pixel-format)
                                                           :width (* 2 width)
                                                           :height (* 2 width)))))

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
             (multiple-value-bind (data w h bittage pixel-format)
                 (load-image (if (probe-file file)
                                 file
                                 (make-pathname :name "zero" :type "png" :defaults dir))
                             T)
               (declare (ignore w h))
               (v:info :test "~a" file)
               (unwind-protect
                    (%gl:tex-sub-image-2d :texture-2d 0 x y r r pixel-format (infer-pixel-type bittage) (coerce-pixel-data data))
                 (free-image-data data))))
           (path (x y)
             (merge-pathnames (make-pathname :name (format NIL "~d,~d" x y) :type "png"
                                             :directory `(:relative ,(princ-to-string s)))
                              dir)))
      (picture (path (+ l 0) (+ u 0)) 0 r)
      (picture (path (+ l s) (+ u 0)) r r)
      (picture (path (+ l s) (+ u s)) r 0)
      (picture (path (+ l 0) (+ u s)) 0 0))
    ;; Draw into the clipmap layer
    (%gl:copy-image-sub-data (gl-name (texture-buffer clipmap)) :texture-2d 0 (/ (- x l) (expt 2 level)) (/ (- y u) (expt 2 level)) 0
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
in vec4 pos;
out vec4 color;

const int firstOctave = 3;
const int octaves = 8;
const float persistence = 0.6;

//Not able to use bit operator like <<, so use alternative noise function from YoYo
//
//https://www.shadertoy.com/view/Mls3RS
//
//And it is a better realization I think
float noise(int x,int y)
{   
    float fx = float(x);
    float fy = float(y);
    
    return 2.0 * fract(sin(dot(vec2(fx, fy) ,vec2(12.9898,78.233))) * 43758.5453) - 1.0;
}

float smoothNoise(int x,int y)
{
    return noise(x,y)/4.0+(noise(x+1,y)+noise(x-1,y)+noise(x,y+1)+noise(x,y-1))/8.0+(noise(x+1,y+1)+noise(x+1,y-1)+noise(x-1,y+1)+noise(x-1,y-1))/16.0;
}

float COSInterpolation(float x,float y,float n)
{
    float r = n*3.1415926;
    float f = (1.0-cos(r))*0.5;
    return x*(1.0-f)+y*f;
    
}

float InterpolationNoise(float x, float y)
{
    int ix = int(x);
    int iy = int(y);
    float fracx = x-float(int(x));
    float fracy = y-float(int(y));
    
    float v1 = smoothNoise(ix,iy);
    float v2 = smoothNoise(ix+1,iy);
    float v3 = smoothNoise(ix,iy+1);
    float v4 = smoothNoise(ix+1,iy+1);
    
   	float i1 = COSInterpolation(v1,v2,fracx);
    float i2 = COSInterpolation(v3,v4,fracx);
    
    return COSInterpolation(i1,i2,fracy);
    
}

float PerlinNoise2D(float x,float y)
{
    float sum = 0.0;
    float frequency =0.0;
    float amplitude = 0.0;
    for(int i=firstOctave;i<octaves + firstOctave;i++)
    {
        frequency = pow(2.0,float(i));
        amplitude = pow(persistence,float(i));
        sum = sum + InterpolationNoise(x*frequency,y*frequency)*amplitude;
    }
    
    return sum;
}

void main(){
  float noise = PerlinNoise2D(pos.x,pos.z);
  float s = 3-(pos.y+noise)/100;
  color = vec4((225-noise*30)/255*s, (191-noise*60)/255*s, (146-noise*80)/255*s, 1);
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
  (let ((args ()))
    (flet ((clipmap (o x y s)
             (push (list "(" "+clone"
                         "-crop" (format NIL "~dx~:*~d+~d+~d!" s x y)
                         "-flatten"
                         "-scale" (format NIL "~dx~d!" n n)
                         "-write" (uiop:native-namestring o)
                         "+delete" ")")
                   args))
           (convert ()
             (push (list "-fill" "black"
                         "-draw" "color 0,0 reset"
                         "-scale" (format NIL "~dx~d!" n n)
                         (format NIL "png64:~a" (uiop:native-namestring (make-pathname :name "zero" :type "png" :defaults output))))
                   args)
             (uiop:run-program (list* "magick" "convert"
                                      (uiop:native-namestring input)
                                      "-background" "black"
                                      "-gravity" "center"
                                      "-alpha" "off"
                                      (loop for arg in (nreverse args)
                                            nconc arg))
                               :error-output T)
             (setf args ())))
      (destructuring-bind (w h) (read-from-string
                                 (uiop:run-program (list "magick" "identify" "-format" "(%w %h)"
                                                         (uiop:native-namestring input))
                                                   :output :string))
        (dotimes (l levels output)
          (let* ((s (* n (expt 2 l)))
                 (o (pathname-utils:subdirectory output (princ-to-string s))))
            (format T "~& Generating level ~d (~d tile~:p)...~%" l (expt (/ w s) 2))
            (ensure-directories-exist o)
            (loop for x from (/ w -2) below (/ w 2) by s
                  do (loop for y from (/ h -2) below (/ h 2) by s
                           for f = (make-pathname :name (format NIL "~d,~d" (+ x xoff) (+ y yoff)) :type "png" :defaults o)
                           do (clipmap f (+ x (/ s 2)) (+ y (/ s 2)) s)))
            (convert)))))))
