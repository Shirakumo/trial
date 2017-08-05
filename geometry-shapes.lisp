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

(defun make-quad-grid (s xc yc)
  (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'vertex) :pack T)
    (loop repeat xc
          for x from (- (* s (/ xc 2))) by s
          do (loop repeat yc
                   for y from (- (* s (/ yc 2))) by s
                   do (vertex :position (vec (+ x) (+ y) +0.0))
                      (vertex :position (vec (+ x) (- y) +0.0))
                      (vertex :position (vec (- x) (- y) +0.0))
                      (vertex :position (vec (- x) (- y) +0.0))
                      (vertex :position (vec (- x) (+ y) +0.0))
                      (vertex :position (vec (+ x) (+ y) +0.0))))))

(defun make-line-grid (s w h)
  (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'vertex :face-length 2) :pack T)
    (let ((w (/ w 2)) (h (/ h 2))
          (ws (/ w s)) (hs (/ h s)))
      (loop for x from (- w) to w by ws
            do (vertex :position (vec x 0.0 (- h)))
               (vertex :position (vec x 0.0 (+ h))))
      (loop for y from (- h) to h by hs
            do (vertex :position (vec (- w) 0.0 y))
               (vertex :position (vec (+ w) 0.0 y))))))
