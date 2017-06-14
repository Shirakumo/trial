(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench cat) texture
    (#p"cat.png"))

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :Teapot01Mesh)

(progn
  (define-shader-subject teapot (vertex-subject textured-subject located-entity rotated-entity)
    ()
    (:default-initargs
     :vertex-array (asset 'workbench 'teapot)
     :texture (asset 'workbench 'cat)))
  
  (maybe-reload-scene))

(define-handler (teapot tick) (ev)
  (incf (vz (rotation teapot)) (/ (random 1.0) 10))
  (incf (vx (rotation teapot)) (/ (random 1.0) 10))
  (incf (vy (rotation teapot)) (/ (random 1.0) 10)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (dotimes (i 200)
        (enter (make-instance 'teapot :location (vec3-random -200 200)) scene))
      (enter (make-instance 'target-camera :location (vec 0 2 -200)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass))
          (pass2 (make-instance 'negative-pass))
          (pass3 (make-instance 'box-blur-pass)))
      (connect (flow:port pass1 'color) (flow:port pass2 'previous-pass) pipeline)
      (connect (flow:port pass2 'color) (flow:port pass3 'previous-pass) pipeline)))

  (maybe-reload-scene))
