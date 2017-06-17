(in-package #:trial)

(define-pool workbench
  :base 'trial)

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      ))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))
