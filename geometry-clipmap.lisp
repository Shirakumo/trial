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
   (texture :initarg :texture :accessor texture)))

(defmethod initialize-instance :after ((clipmap clipmap) &key n)
  (setf (clipmap-block clipmap) (make-asset 'mesh (list (make-clipmap-block n))))
  (setf (clipmap-fixup clipmap) (make-asset 'mesh (list (make-clipmap-fixup n))))
  (let ((trims (make-clipmap-trim n)))
    (setf (clipmap-trims clipmap) (loop for trim in trims
                                      collect (make-asset 'mesh (list trim))))))

(defmethod load progn ((clipmap clipmap))
  (load (clipmap-block clipmap))
  (load (clipmap-fixup clipmap))
  (mapc #'load (clipmap-trims clipmap))
  (load (texture clipmap)))

(defmethod paint ((clipmap clipmap) (pass shader-pass))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d-array (resource (texture clipmap)))
  (let* ((n (slot-value clipmap 'n))
         (w (/ n 10))
         (m (1- (/ (1+ n) 4)))
         (s (/ (1- n)))
         (shader (shader-program-for-pass pass clipmap)))
    (setf (uniform shader "model_matrix") (model-matrix))
    (setf (uniform shader "view_matrix") (view-matrix))
    (setf (uniform shader "projection_matrix") (projection-matrix))
    (setf (uniform shader "ViewerPos") (vec2 0 0))
    (draw-ring clipmap m s shader 0 1.0 0 0)
    (loop for level from 1 below (slot-value clipmap 'levels)
          for scale = 0.5 then (/ scale 2)
          for off = (* s scale -1) then (+ off (* s scale))
          do (draw-ring clipmap m s shader level scale off 1))))

(defun draw-ring (clipmap m s shader level scale off f)
  (setf (uniform shader "level") (float level 0s0))
  (setf (uniform shader "ScaleFactor") (vec4 scale scale off off))
  (setf (uniform shader "FineBlockOrig") (vec4 scale scale (+ 0.5 off) (+ 0.5 off)))
  (setf (uniform shader "FinerBlockOrig") (vec4 1 1 (+ 0.5 off) (+ 0.5 off)))
  (gl:bind-vertex-array (resource (clipmap-fixup clipmap)))
  (%gl:draw-elements :triangles (size (clipmap-fixup clipmap)) :unsigned-int 0)
  (setf (uniform shader "ScaleFactor") (vec4 scale scale off off))
  (setf (uniform shader "FineBlockOrig") (vec4 scale scale (+ 0.5 off) (+ 0.5 off)))
  (setf (uniform shader "FinerBlockOrig") (vec4 1 1 (+ 0.5 off) (+ 0.5 off)))
  (gl:bind-vertex-array (resource (nth f (clipmap-trims clipmap))))
  (%gl:draw-elements :triangles (size (nth f (clipmap-trims clipmap))) :unsigned-int 0)
  (gl:bind-vertex-array (resource (clipmap-block clipmap)))
  (flet ((d (x z)
           (setf (uniform shader "ScaleFactor") (vec4 scale scale (+ (* x scale) off) (+ (* z scale) off)))
           (setf (uniform shader "FinerBlockOrig") (vec4 1 1 (+ 0.5 x off) (+ 0.5 z off)))
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

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform sampler2DArray texture_image;
uniform vec4 ScaleFactor, FineBlockOrig, FinerBlockOrig;
uniform vec2 ViewerPos;
uniform float level;

out float z;
out float a;
out vec2 uv;

void main(){
   vec2 worldPos = position.xz * ScaleFactor.xy + ScaleFactor.zw;
   vec2 uv_inner = position.xz * FinerBlockOrig.xy + FinerBlockOrig.zw;
   vec2 uv_outer = uv_inner / 2; // THIS SHOULD WORK BUT DOES NOT WTFFBBQ

   vec2 alpha = clamp(abs(worldPos/ScaleFactor.xy)*20-8, 0, 1);
   a = max(alpha.x, alpha.y);

   float zi = texture(texture_image, vec3(uv_inner, level)).r;
   float zo = texture(texture_image, vec3(uv_outer, max(level-1, 0))).r;
   z = ((1-a) * zi + a * zo) * 0.5;

   gl_Position = projection_matrix * view_matrix * model_matrix * vec4(worldPos.x, z, worldPos.y, 1.0);
   uv = uv_inner;
}")

(define-class-shader (clipmap :fragment-shader)
  "
in float z;
in float a;
in vec2 uv;
out vec4 color;

uniform vec3 LightDirection = vec3(0.2, -0.8, 0.2);
uniform sampler2DArray texture_image;

void main(){
//    vec3 normal = vec3(0.2, 0.8, 0.2);
//    float s = clamp(dot(normal, LightDirection), 0, 1);
//    color = s * vec4(max(1.0, z+0.2), 0, 0, 1);
    color = texture(texture_image, vec3(uv, 0));
    color.r += a;
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
