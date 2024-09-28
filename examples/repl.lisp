(in-package #:org.shirakumo.fraf.trial.examples)

(define-example repl
  :title "Simple REPL"
  :description "Showcases Trial's built-in simple REPL."
  (!meye (view-matrix))
  (nmortho (projection-matrix) -10 +1270 -700 20 0 1)
  (enter (make-instance 'repl :foreground (vec 1 1 1 1)) scene)
  (enter (make-instance 'render-pass) scene))
