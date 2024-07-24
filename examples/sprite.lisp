(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity simple-sprite (animated-sprite located-entity)
  ())

(define-example sprite
  :title "Animated Sprites"
  (enter (make-instance 'simple-sprite :sprite-data (assets:asset :wolf) :name :sprite) scene)
  (enter (make-instance 'sidescroll-camera :zoom 10.0 :target (node :sprite scene)) scene)
  (enter (make-instance 'render-pass) scene))
