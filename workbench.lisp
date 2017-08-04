(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :TEAPOT01MESH)

(define-asset (workbench cat) texture
    (#p"cat.png"))

(define-shader-subject teapot (vertex-subject colored-subject textured-subject located-entity rotated-entity selectable)
  ((vel :initform (/ (random 1.0) (+ 10 (random 20))) :accessor vel))
  (:default-initargs :vertex-array (asset 'workbench 'teapot)
                     :texture (asset 'workbench 'cat)
                     :rotation (vec 0 0 0)
                     :color (vec4-random 0.2 0.8)
                     :location (vec3-random -80 80)))

(define-handler (teapot tick) (ev)
  (incf (vz (rotation teapot)) (vel teapot)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (dotimes (i 10)
        (enter (make-instance 'teapot) scene))
      (enter (make-instance 'target-camera :location (vec 0 100 100)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
