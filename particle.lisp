#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defparameter *particle-vbo*
  (make-instance 'vertex-buffer :buffer-data
                 (make-array 24 :element-type 'single-float :initial-contents
                             '(+0.5 +0.5 1.0 1.0
                               -0.5 +0.5 0.0 1.0
                               -0.5 -0.5 0.0 0.0
                               -0.5 -0.5 0.0 0.0
                               +0.5 -0.5 1.0 0.0
                               +0.5 +0.5 1.0 1.0))))

(define-shader-entity particle-system (located-entity renderable listener)
  ((vertex-array :initarg :vertex-array :accessor vertex-array)
   (particle-data :accessor particle-data)
   (particle-capacity :initarg :particle-capacity :initform 1000 :accessor particle-capacity)
   (active-particles :initform 0 :accessor active-particles)
   (lifetime :initform 1.0 :accessor lifetime)
   (clock :initarg :clock :initform 0.0 :accessor clock)))

(defmethod initialize-instance :after ((system particle-system) &key)
  (let* ((vbo *particle-vbo*)
         (particle-capacity (particle-capacity system))
         (particle-data (make-array (* 2 particle-capacity) :element-type 'single-float))
         (vio (make-instance 'vertex-buffer :data-usage :stream-draw :buffer-data particle-data))
         (vao (make-instance 'vertex-array :bindings `((,vbo :size 2 :stride ,(* 4 4) :offset 0)
                                                       (,vbo :size 2 :stride ,(* 4 4) :offset 8)
                                                       (,vio :size 1 :stride ,(* 2 4) :offset 0 :instancing 1)
                                                       (,vio :size 1 :stride ,(* 2 4) :offset 4 :instancing 1)))))
    (setf (particle-data system) vio)
    (setf (vertex-array system) vao)
    (dotimes (i particle-capacity)
      (setf (aref particle-data (+ (* 2 i) 0)) 0.0)
      (setf (aref particle-data (+ (* 2 i) 1)) (random 1.0)))))

(defmethod stage :after ((system particle-system) (area staging-area))
  (stage (vertex-array system) area))

(define-handler (particle-system tick) (dt)
  (let* ((vbo (particle-data particle-system))
         (data (buffer-data vbo))
         (clock (clock particle-system))
         (active (active-particles particle-system))
         (life (lifetime particle-system)))
    (setf (clock particle-system) (+ clock (float dt 0f0)))
    (loop with i = 0
          for age = (- clock (aref data (+ 0 (* i 2))))
          while (< i active)
          do (cond ((<= life age)
                    (decf active)
                    (rotatef (aref data (+ 0 (* i 2))) (aref data (+ 0 (* active 2))))
                    (rotatef (aref data (+ 1 (* i 2))) (aref data (+ 1 (* active 2)))))
                   (T
                    (incf i))))
    (when (< active (active-particles particle-system))
      (setf (active-particles particle-system) active)
      (update-buffer-data vbo data :count (* 2 4 active)))))

(defmethod render ((system particle-system) (program shader-program))
  (declare (optimize speed))
  (setf (uniform program "model_matrix") (model-matrix))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (setf (uniform program "clock") (clock system))
  ;; FIXME: Problem: alpha blending fucks up if things aren't drawn in depth sorted order.
  ;;        But since all the info is on the gpu how the heck do we sort?
  (gl:bind-vertex-array (gl-name (vertex-array system)))
  (%gl:draw-arrays-instanced :triangles 0 6 (active-particles system))
  (gl:bind-vertex-array 0))

(defmethod emit ((system particle-system) count)
  (let* ((vbo (particle-data system))
         (data (buffer-data vbo))
         (clock (clock system))
         (active (min (particle-capacity system) (+ (active-particles system) count))))
    (loop for i from (active-particles system) below active
          do (setf (aref data (+ 0 (* i 2))) clock)
             (setf (aref data (+ 1 (* i 2))) (random 1.0)))
    (setf (active-particles system) active)
    (update-buffer-data vbo data :count (* 2 4 active))))

(define-class-shader (particle-system :vertex-shader)
  "layout (location = 0) in vec2 position;
layout (location = 1) in vec2 in_uv;
layout (location = 2) in float start_time;
layout (location = 3) in float in_seed;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform float clock;

out vec2 uv;
out float age;
out float seed;

#define PHI 1.61803398874989484820459
#define PI 3.1415926538

float rand(float n){return fract(sin(n) * 43758.5453123);}

void compute_particle(in float age, in float seed, out vec3 position, out vec2 scale);

void main(){
  uv = in_uv;
  age = clock - start_time;
  seed = in_seed;

  vec2 scale;
  vec3 off;
  compute_particle(age, seed, off, scale);

  // Orient the quad to always face the camera
  vec3 center = (model_matrix * vec4(off, 1)).xyz;
  vec3 camera_right = vec3(view_matrix[0][0], view_matrix[1][0], view_matrix[2][0]);
  vec3 camera_up = vec3(view_matrix[0][1], view_matrix[1][1], view_matrix[2][1]);
  vec3 world_pos = center + camera_right * position.x * scale.x + camera_up * position.y * scale.y;
  gl_Position = projection_matrix * view_matrix * vec4(world_pos, 1.0);
}")

(define-class-shader (particle-system :fragment-shader)
  "
in vec2 uv;
in float age;
in float seed;

out vec4 color;

void main(){
  color = vec4(1);
}")
