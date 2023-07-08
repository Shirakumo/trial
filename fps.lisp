#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;;; Very fast FPS display. Focus is on reducing GPU load.

(define-asset (trial fps-counter) mesh
    (let ((mesh (make-instance 'vertex-mesh :vertex-type 'textured-vertex)))
      (with-vertex-filling (mesh)
        (flet ((rect (x)
                 (vertex :location (vec (+ x 16) 16 0) :uv (vec 0.1 1))
                 (vertex :location (vec (+ x 0)  16 0) :uv (vec   0 1))
                 (vertex :location (vec (+ x 0)   0 0) :uv (vec   0 0))
                 (vertex :location (vec (+ x 0)   0 0) :uv (vec   0 0))
                 (vertex :location (vec (+ x 16)  0 0) :uv (vec 0.1 0))
                 (vertex :location (vec (+ x 16) 16 0) :uv (vec 0.1 1))))
          (loop for i from 0 by 16
                repeat 6
                do (rect i))
          mesh)))
  :data-usage :stream-draw)

(define-asset (trial fps-texture) image
    #p"fps-texture.png"
  :mag-filter :nearest)

(declaim (type (unsigned-byte 60) +frame-count+ +start-time+))
(define-global +frame-count+ 0)
(define-global +start-time+ 0)

(define-shader-entity fps-counter (renderable)
  ((name :initform 'fps-counter)))

(defmethod stage :after ((counter fps-counter) (area staging-area))
  (stage (// 'trial 'fps-counter) area)
  (stage (// 'trial 'fps-texture) area))

(defmethod render ((counter fps-counter) (program shader-program))
  (declare (optimize speed (safety 1)))
  (let* ((vao (// 'trial 'fps-counter))
         (now (get-internal-real-time))
         (dt (- now +start-time+)))
    (incf +frame-count+)
    (when (<= (/ internal-time-units-per-second 10) dt)
      (let* ((fps (floor (/ +frame-count+ (/ dt internal-time-units-per-second))))
             (buf (caadr (bindings vao)))
             (dat (buffer-data buf)))
        (declare (type (simple-array single-float) dat))
        (declare (type (unsigned-byte 32) fps))
        (setf +start-time+ now)
        (setf +frame-count+ 0)
        (flet ((set-rect (i d)
                 (let* ((b (* 5 (* 6 i)))
                        (d0 (/ d 10.0))
                        (d1 (+ d0 0.1)))
                   (setf (aref dat (+ b  3)) d1)
                   (setf (aref dat (+ b  8)) d0)
                   (setf (aref dat (+ b 13)) d0)
                   (setf (aref dat (+ b 18)) d0)
                   (setf (aref dat (+ b 23)) d1)
                   (setf (aref dat (+ b 28)) d1))))
          (loop for i downfrom 5 to 0
                for div = 1 then (* 10 div)
                do (set-rect i (mod (floor fps div) 10))))
        (update-buffer-data buf T)))
    (bind (// 'trial 'fps-texture) NIL)
    (render vao program)))

(define-class-shader (fps-counter :vertex-shader)
  "
layout (location = 0) in vec3 position;
layout (location = 2) in vec2 in_uv;
out vec2 uv;

void main(){
  gl_Position = mat4(0.0015625, 0.0, 0.0, 0.0, 0.0, 0.0027777778, 0.0, 0.0, 0.0, 0.0, -0.02, 0.0, -1.0, -1.0, -1.0, 1.0)
              * vec4(position, 1.0);
  uv = in_uv;
}")

(define-class-shader (fps-counter :fragment-shader)
  "uniform sampler2D texture_image;
in vec2 uv;
out vec4 color;

void main(){
  color = texture(texture_image, uv);
}")
