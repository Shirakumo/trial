(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench cube) mesh
    (make-cube 20))

(define-asset (workbench grid) mesh
    (make-line-grid 10 200 200))

(define-asset (workbench cat) image
    #p"cat.png")

(define-asset (workbench skybox) image
    '(#p"nissi-beach/posx.jpg"
      #p"nissi-beach/negx.jpg"
      #p"nissi-beach/posy.jpg"
      #p"nissi-beach/negy.jpg"
      #p"nissi-beach/posz.jpg"
      #p"nissi-beach/negz.jpg")
  :target :texture-cube-map)

(define-shader-subject cube (vertex-entity colored-entity textured-entity located-entity rotated-entity selectable)
  ((vel :initform (/ (random 1.0) (+ 10 (random 20))) :accessor vel))
  (:default-initargs :vertex-array (asset 'workbench 'cube)
                     :texture (asset 'workbench 'cat)
                     :rotation (vec (/ PI -2) 0 0)
                     :color (vec4-random 0.2 0.8)
                     :location (vx_z (vec3-random -100 100))))

(define-shader-subject grid (vertex-entity colored-entity)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'grid)))

(define-handler (cube tick) (ev)
  (incf (vz (rotation cube)) (vel cube)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (enter (make-instance 'grid) scene)
    (dotimes (i 5)
      (enter (make-instance 'cube) scene))
    (enter (make-instance 'editor-camera :location (vec 0 100 150)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
