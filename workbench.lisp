(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench teapot) mesh
    (#p"teapot.vf")
  :mesh :TEAPOT01MESH)

(define-shader-subject teapot (vertex-subject colored-subject selectable)
  ()
  (:default-initargs :vertex-array (asset 'workbench 'teapot)))

(define-subject clicky ()
  ((buffer :initform NIL :accessor buffer)))

(define-handler (clicky mouse-release) (ev pos)
  (paint (buffer clicky) (buffer clicky))
  (print (object-at-point pos (buffer clicky))))

(defmethod load progn ((clicky clicky))
  (setf (buffer clicky) (load (make-instance 'selection-buffer
                                             :scene *loop*
                                             :width (width *context*)
                                             :height (height *context*)))))

(defmethod offload progn ((clicky clicky))
  (finalize (buffer clicky)))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'teapot) scene)
      (enter (make-instance 'target-camera) scene)
      (enter (make-instance 'clicky) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
