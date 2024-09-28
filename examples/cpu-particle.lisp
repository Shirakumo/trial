(in-package #:org.shirakumo.fraf.trial.examples)

(define-example cpu-particle
  :title "CPU Particle Simulation"
  :description "Playground for the CPU-simulated particle system engine."
  :superclasses (particle-scene)
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'cpu-particle-emitter
                        :name :emitter :max-particles 200 :particle-rate 60
                        :particle-force-fields `((:type :direction :strength -5.0)
                                                 (:type :vortex :strength 10.0))
                        :texture (assets:// :circle-05)
                        :orientation (qfrom-angle +vx+ (deg->rad 90))
                        :particle-options `(:velocity 10.0 :randomness 0.5 :size 0.1 :scaling 1.0
                                            :lifespan 3.0 :lifespan-randomness 0.5)) scene)
  (observe! (live-particles (node :emitter T)) :title "Alive Particles")
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'phong-render-pass) scene))
