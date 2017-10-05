(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-shader-entity thing (vertex-entity)
  ()
  (:default-initargs
   :vertex-array (make-asset 'mesh (list (make-cone 30 50)))))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'thing) scene)
      (enter (make-instance 'editor-camera) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))

