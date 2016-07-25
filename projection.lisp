#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun modelview-matrix ()
  (gl:get-double :modelview-matrix 16))

(defun projection-matrix ()
  (gl:get-double :projection-matrix 16))

(defun proj-matrix (camera)
  (gl:with-pushed-matrix* (:projection)
    (setup-perspective camera (make-instance 'resize :width (width *context*)
                                                     :height (height *context*)))
    (projection-matrix)))

(defun view-matrix (camera)
  (gl:with-pushed-matrix* (:modelview)
    (project-view camera NIL)
    (modelview-matrix)))

(defun 4x4-4v-mult (matrix in)
  (let ((out (v4 0.0 0.0 0.0 0.0)))
    (dotimes (i 4 out)
      (setf (aref out i)
            (+ (* (aref in 0) (aref matrix (+ i (* 4 0))))
               (* (aref in 1) (aref matrix (+ i (* 4 1))))
               (* (aref in 2) (aref matrix (+ i (* 4 2))))
               (* (aref in 3) (aref matrix (+ i (* 4 3)))))))))

(defun vec->4v (vec)
  (v4 (vx vec) (vy vec) (vz vec) 1.0s0))

(defun 4v->vec (4v)
  (vec (aref 4v 0) (aref 4v 1) (aref 4v 2)))

(defun vec->screen (vec modelview projection width height)
  (let ((clip-pos (4x4-4v-mult projection (4x4-4v-mult modelview (vec->4v vec)))))
    (let ((w (aref clip-pos 3)))
      (if (= 0.0s0 w)
          (vec -1 -1 0)
          (let* ((norm-pos (nv+ (nv* (4v->vec clip-pos) (/ 0.5s0 w)) 0.5s0)))
            (vsetf norm-pos
                   (* width (vx norm-pos))
                   (* height (- 1 (vy norm-pos)))
                   0.0s0))))))

(defun vec->main (vec main)
  (with-context (main)
    (vec->screen vec
                 (view-matrix (unit :camera (scene main)))
                 (proj-matrix (unit :camera (scene main)))
                 (q+:width main)
                 (q+:height main))))
