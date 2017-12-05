(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :TEAPOT01MESH)

(define-asset (workbench grid) mesh
    ((make-line-grid 10 200 200)))

(define-asset (workbench cat) texture
    (#p"cat.png"))

(define-asset (workbench skybox) texture
    (#p"nissi-beach/posx.jpg"
     #p"nissi-beach/negx.jpg"
     #p"nissi-beach/posy.jpg"
     #p"nissi-beach/negy.jpg"
     #p"nissi-beach/posz.jpg"
     #p"nissi-beach/negz.jpg")
  :target :texture-cube-map)

(define-shader-subject teapot (vertex-entity colored-entity textured-entity located-entity rotated-entity selectable)
  ((vel :initform (/ (random 1.0) (+ 10 (random 20))) :accessor vel))
  (:default-initargs :vertex-array (asset 'workbench 'teapot)
                     :texture (asset 'workbench 'cat)
                     :rotation (vec (/ PI -2) 0 0)
                     :color (vec4-random 0.2 0.8)
                     :location (vx_z (vec3-random -100 100))))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)
                     :vertex-form :lines))

(define-handler (teapot tick) (ev)
  (incf (vz (rotation teapot)) (vel teapot)))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (enter (make-instance 'grid) scene)
    (dotimes (i 5)
      (enter (make-instance 'teapot) scene))
    (enter (make-instance 'editor-camera :location (vec 0 100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
