(in-package #:org.shirakumo.fraf.trial.examples)

(define-example debug
  :title "Debug Drawing"
  :description "Illustrates the on-the-fly debug drawing utilities in Trial"
  ;; Not necessary usually, any of the debug-* commands automatically do
  ;; enter the debug-draw instance for you as needed.
  (enter (make-instance 'debug-draw :clear-after-render NIL) scene)
  (enter (make-instance 'editor-camera :location (vec 0 0 30) :move-speed 0.1) scene)
  (enter (make-instance 'render-pass :clear-color (vec4 0.2 0.23 0.3 1)) scene)
  (let ((colors (list (vec 1 1 1) (vec 1 0 0) (vec 0 1 0) (vec 0 0 1)
                      (vec 1 1 0) (vec 0 1 1) (vec 1 0 1) (vec 0 0 0))))
    (nconc colors colors)
    (flet ((next-color ()
             (pop colors)))
      (debug-point (vec3 -30 +10 0))
      (debug-line (vec -22 +7 0) (vec -18 +13 0) :color (next-color))
      (debug-matrix (nmtranslate (mrotation +vx3+ F-PI/4) (vec 10 -10 0)) :color (next-color))
      (debug-vector (vec 0 10 0) (vec -0.5 11 +0.5) :color (next-color))
      (debug-orientation (vec 10 10 0) (qfrom-angle +vx+ F-PI/4) :stretch (vec3 3))
      (debug-text (vec +20 10 0) "Hello!" :scale 0.1)

      (debug-box (vec -30 0 0) (vec3 2) :color (next-color))
      (debug-sphere (vec -20 0 0) 2 :color (next-color))
      (debug-ellipsoid (vec -10 0 0) (vec3 2 3 1) :color (next-color))
      (debug-cylinder (vec 0 0 0) 2 2 :color (next-color))
      (debug-cone (vec +10 0 0) 2 2 :color (next-color))
      (debug-pill (vec +20 0 0) 2 2 :color (next-color))
      (debug-draw (make-tube-mesh 2 4 1 :x +30 :y -2 :segments 8) :color (next-color))

      (debug-draw (make-cube-mesh 4 :x -30 :y -10) :flats T :color (next-color))
      (debug-draw (make-sphere-mesh 2 :x -20 :y -10 :segments 8) :flats T :color (next-color))
      (debug-draw (make-sphere-mesh '(2 3 1) :x -10 :y -10 :segments 8) :flats T :color (next-color))
      (debug-draw (make-cylinder-mesh 2 4 :x 0 :y -12 :segments 8) :flats T :color (next-color))
      (debug-draw (make-cone-mesh 2 4 :x +10 :y -12 :segments 8) :flats T :color (next-color))
      (debug-draw (make-triangle-mesh 2 4 :x +20 :y -10) :flats T :color (next-color))
      (debug-draw (make-tube-mesh 2 4 1 :x +30 :y -12 :segments 16) :flats T :color (next-color)))))
