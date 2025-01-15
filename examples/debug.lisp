(in-package #:org.shirakumo.fraf.trial.examples)

(define-example debug
  :title "Debug Drawing"
  :description "Illustrates the on-the-fly debug drawing utilities in Trial"
  ;; Not necessary usually, any of the debug-* commands automatically do
  ;; for you as needed.
  (enter (make-instance 'editor-camera :location (vec 0 0 30) :move-speed 0.1) scene)
  (enter (make-instance 'debug-draw :clear-after-render NIL) scene)
  (enter (make-instance 'render-pass :clear-color (vec4 0.2 0.23 0.3 1)) scene)
  (debug-point (vec3 0))
  (debug-line (vec -1 1 0) (vec +1 1 0) :color (vec 0 0 1))
  (debug-text (vec 0 -30 0) "Hello!" :scale 0.1)
  (debug-draw (transform (vec 10 0 0) (vec3 3) (qfrom-angle +vx+ F-PI/4)))
  (debug-box (vec -30 -10 0) (vec3 2))
  (debug-sphere (vec -20 -10 0) 2)
  (debug-ellipsoid (vec -10 -10 0) (vec3 2 5 1))
  (debug-cylinder (vec 0 -10 0) 2 2)
  (debug-cone (vec +10 -10 0) 2 2)
  (debug-pill (vec +20 -10 0) 2 2)
  (debug-draw (make-tube-mesh 2 4 1 :x +30 :y -12 :segments 8))
  (debug-draw (make-cube-mesh 4 :x -30 :y +10) :flats T)
  (debug-draw (make-sphere-mesh 2 :x -20 :y +10 :segments 8) :flats T)
  (debug-draw (make-sphere-mesh '(2 5 1) :x -10 :y +10 :segments 8) :flats T)
  (debug-draw (make-cylinder-mesh 2 4 :x 0 :y +8 :segments 8) :flats T)
  (debug-draw (make-cone-mesh 2 4 :x +10 :y +8 :segments 8) :flats T)
  (debug-draw (make-triangle-mesh 2 4 :x +20 :y +10) :flats T)
  (debug-draw (make-tube-mesh 2 4 1 :x +30 :y +8 :segments 16) :flats T))
