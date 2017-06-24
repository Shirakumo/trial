#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial fullscreen-square) packed-vertex-array
    (#(0 1 2 2 3 0)
     3 #(+1.0 +1.0 +0.0
         +1.0 -1.0 +0.0
         -1.0 -1.0 +0.0
         -1.0 +1.0 +0.0)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

(defun make-rectangle (w h)
  (let ((w (/ w 2)) (h (/ h 2)))
    (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'textured-vertex) :pack T)
      (vertex :position (vec (+ w) (+ h) +0.0) :uv (vec 1.0 1.0))
      (vertex :position (vec (+ w) (- h) +0.0) :uv (vec 1.0 0.0))
      (vertex :position (vec (- w) (- h) +0.0) :uv (vec 0.0 0.0))
      (vertex :position (vec (- w) (- h) +0.0) :uv (vec 0.0 0.0))
      (vertex :position (vec (- w) (+ h) +0.0) :uv (vec 0.0 1.0))
      (vertex :position (vec (+ w) (+ h) +0.0) :uv (vec 1.0 1.0)))))

(defun make-cube (s)
  (let ((s (/ s 2)))
    (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'textured-vertex) :pack T)
      (vertex :position (vec (+ s) (+ s) (- s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- s) (+ s) (- s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- s) (+ s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (+ s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (+ s) (+ s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ s) (+ s) (- s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ s) (- s) (+ s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- s) (- s) (+ s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- s) (- s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (- s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (- s) (- s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ s) (- s) (+ s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ s) (+ s) (+ s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- s) (+ s) (+ s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- s) (- s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (- s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (- s) (+ s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ s) (+ s) (+ s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ s) (- s) (- s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- s) (- s) (- s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- s) (+ s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (+ s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (+ s) (- s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ s) (- s) (- s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (- s) (+ s) (+ s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- s) (+ s) (- s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- s) (- s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (- s) (- s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- s) (- s) (+ s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (- s) (+ s) (+ s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ s) (+ s) (- s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (+ s) (+ s) (+ s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (+ s) (- s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (- s) (+ s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ s) (- s) (- s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ s) (+ s) (- s)) :uv (vec 1.0 1.0)))))
