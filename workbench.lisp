(in-package #:trial)
(in-readtable :qtools)

(define-asset texture-asset (radiance av)
              #p"~/av.png")

(define-asset packed-vao-asset (radiance cube)
              (#(0 1 2 2 3 0
                 0 1 5 5 4 0
                 2 3 7 7 6 2
                 4 5 6 6 7 4)
                3 #(+0.5 +0.5 +0.5
                    +0.5 -0.5 +0.5
                    -0.5 -0.5 +0.5
                    -0.5 +0.5 +0.5
                    +0.5 +0.5 -0.5
                    +0.5 -0.5 -0.5
                    -0.5 -0.5 -0.5
                    -0.5 +0.5 -0.5)
                2 #(1.0 1.0
                    1.0 0.0
                    0.0 0.0
                    0.0 1.0
                    1.0 1.0
                    1.0 0.0
                    0.0 0.0
                    0.0 1.0)))

(define-shader-subject testcube (vertex-subject textured-subject)
  ()
  (:default-initargs
   :texture (asset 'radiance 'av)
   :vertex-array (asset 'radiance 'cube)))

(defvar *pipeline*)

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (enter (make-instance 'testcube) scene)
    (load scene)
    (setf *pipeline* (make-instance 'pipeline))
    (register (make-instance 'per-object-pass) *pipeline*)
    (pack-pipeline *pipeline* main)
    (load *pipeline*)))

(defmethod paint ((source main) (target main))
  (let ((scene (scene source)))
    (gl:viewport 0 0 (width target) (height target))
    (issue scene 'tick)
    (process scene)
    (reset-matrix (projection-matrix))
    (reset-matrix (view-matrix))
    (perspective-projection 45 (/ (width target) (height target)) 0.1 100)
    (translate-by 0 0 -3 (view-matrix))
    (rotate +vx+ 0.03)
    (rotate +vy+ 0.05)
    (rotate +vz+ 0.07)
    (paint *pipeline* target)))
