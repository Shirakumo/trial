#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric debug-draw (thing &key))

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

(defmacro define-debug-draw-function ((name type) args &body body)
  `(defun ,name (,@args (debug-draw (node 'debug-draw T)) (update T))
     (unless debug-draw
       (setf debug-draw (enter-and-load (make-instance 'debug-draw) (scene +main+) +main+)))
     (let ((data (,type debug-draw)))
       (flet ((v (v)
                (vector-push-extend (vx v) data)
                (vector-push-extend (vy v) data)
                (vector-push-extend (vz v) data)))
         ,@body))
     (when update
       (resize-buffer (caar (bindings (,(ecase type (points 'points-vao) (lines 'lines-vao)) debug-draw))) T))))

(defmethod debug-draw ((point vec2) &rest args)
  (apply #'debug-point (vxy_ point) args))

(defmethod debug-draw ((point vec3) &rest args)
  (apply #'debug-point point args))

(define-debug-draw-function (debug-point points) (point &key (color #.(vec 1 0 0)))
  (v point)
  (v color))

(define-debug-draw-function (debug-line lines) (a b &key (color-a #.(vec 1 0 0)) (color-b #.(vec 0 0 0)))
  (v a)
  (v color-a)
  (v b)
  (v color-b))

(defmethod debug-draw ((entity vertex-entity) &rest args)
  (apply #'debug-vertex-array (vertex-array entity) args))

(defmethod debug-draw :around ((entity transformed-entity) &rest args)
  (unless (getf args :transform)
    (setf (getf args :transform) (tmat4 (tf entity))))
  (apply #'call-next-method entity args))

(define-debug-draw-function (debug-vertex-array lines) (vao &key (color #.(vec 1 0 0)) (transform (model-matrix)))
  (let ((count 0) prev pprev)
    (labels ((lines (vec)
               (v vec)
               (v color))
             (line-strip (vec)
               (case count
                 (0 (lines vec)
                  (setf count 1))
                 (1 (lines vec)
                  (lines vec))))
             (line-loop (vec)
               (error "Not implemented"))
             (triangles (vec)
               (case count
                 (0 (lines vec)
                  (setf prev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (lines vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (setf count 0))))
             (triangle-strip (vec)
               (case count
                 (0 (lines vec)
                  (setf pprev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (setf prev vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (lines pprev)
                  (shiftf pprev prev vec))))
             (triangle-fan (vec)
               (case count
                 (0 (lines prev)
                  (setf pprev vec)
                  (setf count 1))
                 (1 (lines vec)
                  (setf prev vec)
                  (setf count 2))
                 (T (lines vec)
                  (lines prev)
                  (lines vec)
                  (lines pprev)
                  (setf prev vec)))))
      (let ((vertex (ecase (vertex-form vao)
                      (:lines #'lines)
                      (:line-strip #'line-strip)
                      (:line-loop #'line-loop)
                      (:triangles #'triangles)
                      (:triangle-strip #'triangle-strip)
                      (:triangle-fan #'triangle-fan)))
            ebo vbo)
        (loop for binding in (bindings vao)
              do (case (buffer-type (unlist binding))
                   (:element-array-buffer
                    (setf ebo (buffer-data (unlist binding))))
                   (:array-buffer
                    (when (= 0 (getf (rest binding) :index 0))
                      (setf vbo binding)))))
        (destructuring-bind (buffer &key (size 3) (stride 0) (offset 0) &allow-other-keys) vbo
          (let ((data (buffer-data buffer)))
            (cond (ebo
                   (loop for e across ebo
                         for i = (+ (floor offset 4) (* (floor stride 4) e))
                         do (ecase size
                              (3
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) (aref data (+ 2 i)) 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))
                              (2
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) 0.0 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec))))))
                  (T
                   (loop for i from (floor offset 4) by (floor stride 4) below (length data)
                         do (ecase size
                              (3
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) (aref data (+ 2 i)) 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))
                              (2
                               (let ((vec (vec (aref data (+ 0 i)) (aref data (+ 1 i)) 0.0 1.0)))
                                 (declare (dynamic-extent vec))
                                 (n*m transform vec)
                                 (funcall vertex vec)))))))))))))

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
