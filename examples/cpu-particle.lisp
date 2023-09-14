(in-package #:org.shirakumo.fraf.trial.examples)

(define-example cpu-particle
  (gl:clear-color 0 0 0 0)
  (enter (make-instance '3d-camera :location (vec 0 0 -5)) scene)
  (enter (make-instance 'cpu-particle-emitter
                        :name :emitter :max-particles 100 :particle-rate 10
                        :texture (assets:// :circle-05)
                        :vertex-array (// 'trial 'unit-point)
                        :particle-options `(:velocity -5.0 :randomness 0.0 :size 0.1 :scaling 1.0
                                            :lifespan 1.0 :lifespan-randomness 0.0)) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'phong-render-pass) scene))
