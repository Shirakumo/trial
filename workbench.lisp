(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :TEAPOT01MESH)

(define-shader-subject teapot (vertex-subject colored-subject)
  ()
  (:Default-initargs :vertex-array (asset 'workbench 'teapot)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'teapot) scene)
      (enter (make-instance 'target-camera) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
