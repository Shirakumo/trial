(in-package #:org.shirakumo.fraf.trial.examples)

(define-example tilemap
  :title "Tile Maps"
  (enter (make-instance 'tile-layer :tile-data (assets:asset :tilemap) :name :tilemap) scene)
  (enter (make-instance 'sidescroll-camera :zoom 2.0 :target (node :sprite scene)) scene)
  (enter (make-instance 'render-pass) scene))
