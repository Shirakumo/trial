(in-package #:trial)
(in-readtable :qtools)

(define-shader-subject testcube (vertex-subject textured-subject)
  ()
  (:default-initargs
   :texture (make-asset 'texture-asset #p"~/av.png")
   :vertex-array (pack-vao #(3 1 0 3 2 1)
                  3 #(+0.5 +0.5 0.0
                      +0.5 -0.5 0.0
                      -0.5 -0.5 0.0
                      -0.5 +0.5 0.0)
                  2 #(1.0 1.0
                      1.0 0.0
                      0.0 0.0
                      0.0 1.0))))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (enter (make-instance 'testcube) scene)))

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
    (with-pushed-matrix
      (translate-by 0 0 -0.5)
      (rotate +vy+ (* pi 1/180 180))
      (paint scene target))
    (with-pushed-matrix
      (translate-by 0 0 0.5)
      (rotate +vy+ (* pi 1/180 0))
      (paint scene target))
    (with-pushed-matrix
      (translate-by -0.5 0 0)
      (rotate +vy+ (* pi 1/180 -90))
      (paint scene target))
    (with-pushed-matrix
      (translate-by 0.5 0 0)
      (rotate +vy+ (* pi 1/180 90))
      (paint scene target))
    (with-pushed-matrix
      (translate-by 0 0.5 0)
      (rotate +vx+ (* pi 1/180 -90))
      (paint scene target))
    (with-pushed-matrix
      (translate-by 0 -0.5 0)
      (rotate +vx+ (* pi 1/180 90))
      (paint scene target))))
