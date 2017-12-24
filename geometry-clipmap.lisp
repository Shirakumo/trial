#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-subject clipmap ()
  ((n :initarg :n)
   (levels :initarg :levels)
   (clipmap-block :accessor clipmap-block)
   (clipmap-fixup :accessor clipmap-fixup)
   (clipmap-trims :accessor clipmap-trims)
   (clipmap-center :accessor clipmap-center)
   (texture :initarg :texture :accessor texture)))

(defmethod initialize-instance :after ((clipmap clipmap) &key n)
  (setf (clipmap-center clipmap) (make-asset 'mesh (list (make-quad-grid (/ (1- n)) (1- n) (1- n)))))
  (setf (clipmap-block clipmap) (make-asset 'mesh (list (make-clipmap-block n))))
  (setf (clipmap-fixup clipmap) (make-asset 'mesh (list (make-clipmap-fixup n))))
  (let ((trims (make-clipmap-trim n)))
    (setf (clipmap-trims clipmap) (loop for trim in trims
                                      collect (make-asset 'mesh (list trim))))))

(defmethod paint ((clipmap clipmap) (pass shader-pass))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d-array (resource (texture clipmap)))
  (let* ((n (slot-value clipmap 'n))
         (w (/ n 10))
         (m (1- (/ (1+ n) 4)))
         (s (/ (1- n)))
         (shader (shader-program-for-pass pass clipmap)))
    (setf (uniform shader "view_matrix") (view-matrix))
    (setf (uniform shader "projection_matrix") (projection-matrix))
    (loop for scale = 1 then (/ scale 2)
          for offp = 0 then off
          for off = 0 then (+ off (* s scale))
          for level from 0 below (1- (slot-value clipmap 'levels))
          do (draw-ring clipmap m s shader level scale off offp 1)
          finally (setf (uniform shader "level") (float level 0s0))
                  (setf (uniform shader "scale") (float scale 0s0))
                  (setf (uniform shader "offset") (float off 0s0))
                  (setf (uniform shader "offsetp") (float offp 0s0))
                  (setf (uniform shader "block") (vec 0 0))
                  (gl:bind-vertex-array (resource (clipmap-center clipmap)))
                  (%gl:draw-elements :triangles (size (clipmap-center clipmap)) :unsigned-int 0))))

(defun draw-ring (clipmap m s shader level scale off offp f)
  (setf (uniform shader "level") (float level 0s0))
  (setf (uniform shader "scale") (float scale 0s0))
  (setf (uniform shader "offset") (float off 0s0))
  (setf (uniform shader "offsetp") (float offp 0s0))
  (setf (uniform shader "block") (vec 0 0))
  (gl:bind-vertex-array (resource (clipmap-fixup clipmap)))
  (%gl:draw-elements :triangles (size (clipmap-fixup clipmap)) :unsigned-int 0)
  (gl:bind-vertex-array (resource (nth f (clipmap-trims clipmap))))
  (%gl:draw-elements :triangles (size (nth f (clipmap-trims clipmap))) :unsigned-int 0)
  (gl:bind-vertex-array (resource (clipmap-block clipmap)))
  (flet ((d (x z)
           (setf (uniform shader "block") (vec x z))
           (%gl:draw-elements :triangles (size (clipmap-block clipmap)) :unsigned-int 0)))
    (d (- 0.5 (* s m 0.5)) (- 0.5 (* s m 0.5)))
    (d (- 0.5 (* s m 0.5)) (- 0.5 (* s m 1.5)))
    (d (- 0.5 (* s m 0.5)) (- 0.5 (* s m 1.5)))
    (d (- 0.5 (* s m 0.5)) (- (* s m 1.5) 0.5))
    (d (- 0.5 (* s m 0.5)) (- (* s m 0.5) 0.5))
    (d (- (* s m 0.5) 0.5) (- 0.5 (* s m 0.5)))
    (d (- (* s m 0.5) 0.5) (- 0.5 (* s m 1.5)))
    (d (- (* s m 0.5) 0.5) (- 0.5 (* s m 1.5)))
    (d (- (* s m 0.5) 0.5) (- (* s m 1.5) 0.5))
    (d (- (* s m 0.5) 0.5) (- (* s m 0.5) 0.5))
    (d (- (* s m 1.5) 0.5) (- (* s m 0.5) 0.5))
    (d (- (* s m 1.5) 0.5) (- 0.5 (* s m 0.5)))
    (d (- 0.5 (* s m 1.5)) (- 0.5 (* s m 0.5)))
    (d (- 0.5 (* s m 1.5)) (- (* s m 0.5) 0.5))))

(define-class-shader (clipmap :vertex-shader)
  "layout (location = 0) in vec3 position;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray texture_image;
uniform float level, scale, offset, offsetp;
uniform vec2 block;

out vec3 normal;
out float z;

void main(){
   float level_o = max(level-1, 0);
   vec2 world = (position.xz + block) * scale + offset;
   vec2 uv_inner = position.xz + block + offset + 0.5;
   vec2 uv_outer = (position.xz + block + offsetp)/2 + offset + 0.5;

   vec2 alpha = clamp(abs(world/scale)*10-3.8, 0, 1);
   float a = max(alpha.x, alpha.y);
   if(level == 0) a = 0;

   float zi = texture(texture_image, vec3(uv_inner, level)).r;
   float zo = texture(texture_image, vec3(uv_outer, level_o)).r;
   z = ((1-a) * zi + a * zo) * 0.3;

   gl_Position = projection_matrix * view_matrix * vec4(world.x, z, world.y, 1.0);

   // deduce terrain normal
   vec3 off = vec3(0.0078125, 0.0078125, 0.0);
   float hLi = texture(texture_image, vec3(uv_inner - off.xz, level)).r*100;
   float hRi = texture(texture_image, vec3(uv_inner + off.xz, level)).r*100;
   float hDi = texture(texture_image, vec3(uv_inner - off.zy, level)).r*100;
   float hUi = texture(texture_image, vec3(uv_inner + off.zy, level)).r*100;
   float hLo = texture(texture_image, vec3(uv_outer - off.xz, level_o)).r*100;
   float hRo = texture(texture_image, vec3(uv_outer + off.xz, level_o)).r*100;
   float hDo = texture(texture_image, vec3(uv_outer - off.zy, level_o)).r*100;
   float hUo = texture(texture_image, vec3(uv_outer + off.zy, level_o)).r*100;
   float hL = ((1-a) * hLi + a * hLo);
   float hR = ((1-a) * hRi + a * hRo);
   float hD = ((1-a) * hDi + a * hDo);
   float hU = ((1-a) * hUi + a * hUo);

   normal.x = hL - hR;
   normal.y = hD - hU;
   normal.z = 2.0;
   normal = normalize(normal);
}")

(define-class-shader (clipmap :fragment-shader)
  "
uniform vec3 light = vec3(0.5, 1.0, 2.0);
uniform sampler2DArray texture_image;

in vec3 normal;
in float z;
out vec4 color;

void main(){
    float v = z*3+abs(normal.y)/2.5;
    float s = clamp(dot(normal, normalize(light)), 0, 1);
    vec3 c = vec3(1,1,1);
    if(v < 0.5){
      c = vec3(0.2, 0.7, 0.2);
    }else if(v < 0.75){
      c = mix(vec3(0.2, 0.7, 0.2), vec3(0.6,0.6,0.6), (v-0.5)/0.25);
    }else if(v < 0.9){
      c = mix(vec3(0.6,0.6,0.6), vec3(1.0,1.0,1.0), (v-0.75)/0.15);
    }else{
      c = vec3(1,1,1);
    }
    color = vec4(s * c, 1.0);
}")

(defun make-clipmap-block (n)
  (let ((m (1- (/ (1+ n) 4)))
        (s (/ (1- n))))
    (make-quad-grid s m m)))

(defun make-clipmap-fixup (n)
  (let ((m (1- (/ (1+ n) 4)))
        (s (/ (1- n)))
        (mesh (make-instance 'vertex-mesh)))
    (make-quad-grid s m 2 :x (- 0.5 (* s m 0.5)) :z 0 :mesh mesh)
    (make-quad-grid s 2 m :x 0 :z (- 0.5 (* s m 0.5)) :mesh mesh)
    (make-quad-grid s m 2 :x (- (* s m 0.5) 0.5) :z 0 :mesh mesh)
    (make-quad-grid s 2 m :x 0 :z (- (* s m 0.5) 0.5) :mesh mesh)))

(defun make-clipmap-trim (n)
  (let ((m (1- (/ (1+ n) 4)))
        (s (/ (1- n)))
        (ne (make-instance 'vertex-mesh))
        (sw (make-instance 'vertex-mesh)))
    (make-quad-grid s (* 2 (1+ m)) 1 :x 0 :z (* s (+ m 0.5)) :mesh ne)
    (make-quad-grid s 1 (* 2 (1+ m)) :x (* s (+ m 0.5)) :z 0 :mesh ne)
    (make-quad-grid s (* 2 (1+ m)) 1 :x 0 :z (* s -1 (+ m 0.5)) :mesh sw)
    (make-quad-grid s 1 (* 2 (1+ m)) :x (* s -1 (+ m 0.5)) :z 0 :mesh sw)
    (list ne sw)))
