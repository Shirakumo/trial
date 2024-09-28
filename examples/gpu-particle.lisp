(in-package #:org.shirakumo.fraf.trial.examples)

(define-example gpu-particle
  :title "GPU Particle Simulation"
  :description "Playground for the GPU-simulated particle system engine. Requires support for compute shaders (OpenGL 4.3+)."
  :superclasses (particle-scene)
  :test (gl-extension-p :gl-arb-compute-shader)
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'gpu-particle-emitter
                        :name :emitter :max-particles 1000000 :particle-rate 10000
                        :particle-force-fields `((:type :direction :strength -5.0)
                                                 (:type :vortex :strength 10.0))
                        :texture (assets:// :circle-05)
                        :orientation (qfrom-angle +vx+ (deg->rad 90))
                        :particle-options `(:velocity -10.0 :randomness 0.5 :size 0.1 :scaling 1.0
                                            :lifespan 10.0 :lifespan-randomness 0.5
                                            :color ,(vec 0.5 0.3 0.1))) scene)
  (observe! (live-particles (node :emitter T)) :title "Alive Particles")
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
  ;; Need a standard render pass here because we need the standard-environment-information.
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'ward)))
    (when (typep (node :emitter scene) 'trial::depth-colliding-particle-emitter)
      (connect render (node :emitter scene) scene))
    (connect (port render 'color) (port map 'previous-pass) scene)))
