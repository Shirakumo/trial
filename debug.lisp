#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity debug-draw (renderable)
  ((name :initform 'debug-draw)
   (points-vao :accessor points-vao)
   (points :accessor points)
   (lines-vao :accessor lines-vao)
   (lines :accessor lines)))

(defmethod initialize-instance :after ((draw debug-draw) &key)
  (setf (points draw) (make-array 64 :fill-pointer 0 :adjustable T :element-type 'single-float))
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (points draw))))
    (setf (points-vao draw) (make-instance 'vertex-array :vertex-form :points :bindings
                                           `((,vbo :offset  0 :stride 24)
                                             (,vbo :offset 12 :stride 24)))))
  (setf (lines draw) (make-array 128 :fill-pointer 0 :adjustable T :element-type 'single-float))
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (lines draw))))
    (setf (lines-vao draw) (make-instance 'vertex-array :vertex-form :lines :bindings
                                          `((,vbo :offset  0 :stride 24)
                                            (,vbo :offset 12 :stride 24))))))

(defmethod stage ((draw debug-draw) (area staging-area))
  (stage (points-vao draw) area)
  (stage (lines-vao draw) area))

(defmethod render ((draw debug-draw) (program shader-program))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (gl:bind-vertex-array (gl-name (points-vao draw)))
  (gl:draw-arrays :points 0 (/ (length (points draw)) 6))
  (gl:bind-vertex-array (gl-name (lines-vao draw)))
  (gl:draw-arrays :lines 0 (/ (length (lines draw)) 6)))

(flet ((v (v a)
         (vector-push-extend (vx v) a)
         (vector-push-extend (vy v) a)
         (vector-push-extend (vz v) a)))
  (defun debug-point (point &key (color #.(vec 1 0 0)) (debug-draw (node 'debug-draw T)) (update T))
    (unless debug-draw
      (setf debug-draw (enter-and-load (make-instance 'debug-draw) (scene +main+) +main+)))
    (let ((data (points debug-draw)))
      (v point data)
      (v color data))
    (when update
      (resize-buffer (caar (bindings (points-vao debug-draw))) T)))

  (defun debug-line (a b &key (color-a #.(vec 1 0 0)) (color-b #.(vec 0 0 0)) (debug-draw (node 'debug-draw T)) (update T))
    (unless debug-draw
      (setf debug-draw (enter-and-load (make-instance 'debug-draw) (scene +main+) +main+)))
    (let ((data (lines debug-draw)))
      (v a data)
      (v color-a data)
      (v b data)
      (v color-b data))
    (when update
      (resize-buffer (caar (bindings (lines-vao debug-draw))) T))))

(defun debug-clear (&key (debug-draw (node 'debug-draw T)) (update T))
  (when debug-draw
    (setf (fill-pointer (points debug-draw)) 0)
    (setf (fill-pointer (lines debug-draw)) 0)
    (when update
      (resize-buffer (caar (bindings (points-vao debug-draw))) T)
      (resize-buffer (caar (bindings (lines-vao debug-draw))) T))))

(define-class-shader (debug-draw :vertex-shader)
  "layout (location = 0) in vec3 position;
layout (location = 1) in vec3 i_color;
out vec3 v_color;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

void main(){
  gl_Position = projection_matrix * view_matrix * vec4(position, 1.0f);
  v_color = i_color;
}")

(define-class-shader (debug-draw :fragment-shader)
  "in vec3 v_color;
out vec4 color;

void main(){
  color = vec4(v_color, 1);
}")
