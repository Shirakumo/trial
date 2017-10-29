(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench heightmap) texture
    (#p"/home/linus/output.png"))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (gl:polygon-mode :front-and-back :line)
      (enter (make-instance 'clipmap :n 39 :texture (asset 'workbench 'heightmap)) scene)
      (enter (make-instance 'vertex-entity :vertex-array (make-asset 'mesh (list (make-cone 0.0001 0.5 :segments 16)))) scene)
      (enter (make-instance 'vertex-entity :vertex-array (make-asset 'mesh (list (make-quad-grid 1 1 1)))) scene)
      ;; (enter (make-instance 'vertex-entity :vertex-array (make-asset 'mesh (list (make-quad-grid (/ 16) 4 2)))) scene)
      (enter (make-instance 'editor-camera :move-speed 0.001 :location (vec 0 0.2 0)) scene)
      ;;(enter (make-instance 'target-camera :target (vec 0 0 0) :location (vec 0 0.6 0.0000001)) scene)
      ))

  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
