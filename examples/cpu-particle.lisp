(in-package #:org.shirakumo.fraf.trial.examples)

(define-example cpu-particle
  :title "CPU Particle Simulation"
  (gl:clear-color 0 0 0 0)
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

(defmethod setup-ui ((scene cpu-particle-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 120 200) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (emitter (node :emitter scene))
        (row -1))
    (macrolet ((wheel (place title start end &rest args)
                 `(progn
                    (alloy:enter ,title layout :row (incf row) :col 1)
                    (alloy:represent (,place emitter) 'alloy:ranged-wheel
                                     :range '(,start . ,end) ,@args :layout-parent layout :focus-parent focus))))
      (let* ((burst 10)
             (button (make-instance 'alloy:button* :value "Burst" :focus-parent focus :on-activate
                                    (lambda () (emit emitter burst)))))
        (alloy:enter button layout :row (incf row) :col 1)
        (alloy:represent burst 'alloy:ranged-wheel :range '(1 . 200) :layout-parent layout :focus-parent focus))
      (wheel particle-rate "Particle Rate" 0 200)
      (wheel particle-lifespan "Lifespan" 0.0 100.0)
      (wheel particle-lifespan-randomness "Lifespan Random" 0.0 1.0)
      (wheel particle-velocity "Velocity" 0.0 100.0)
      (wheel particle-randomness "Randomness" 0.0 1.0)
      (wheel particle-size "Size" 0.01 10.0)
      (wheel particle-scaling "Scaling" 0.0 10.0)
      (wheel particle-rotation "Rotation" 0.0 10.0)
      (wheel particle-motion-blur "Motion Blur" 0.0 1.0)
      (alloy:enter "Texture" layout :row (incf row) :col 1)
      (alloy:represent (texture emitter) T :layout-parent layout :focus-parent focus)
      (alloy:enter "Display Mode" layout :row (incf row) :col 1)
      (alloy:represent (particle-mode emitter) 'alloy:combo-set
                       :value-set '(:quad :billboard) :layout-parent layout :focus-parent focus)
      (alloy:enter "Blend Mode" layout :row (incf row) :col 1)
      (alloy:represent (blend-mode emitter) 'alloy:combo-set
                       :value-set '(:add :normal :invert :darken :multiply :screen) :layout-parent layout :focus-parent focus)
      (alloy:enter "Texture Flip" layout :row (incf row) :col 1)
      (alloy:represent (particle-flip emitter) 'alloy:combo-set
                       :value-set '(NIL :x :y T) :layout-parent layout :focus-parent focus)
      (alloy:enter "Color" layout :row (incf row) :col 1)
      (let* ((color (particle-color emitter))
             (c (alloy:represent color T :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (v c)
          (setf (particle-color emitter) color)))
      (alloy:enter "Emitter Shape" layout :row (incf row) :col 1)
      (let* ((shape :square)
             (c (alloy:represent shape 'alloy:combo-set
                                 :value-set '(:square :disc :sphere :cube) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (v c)
          (setf (vertex-array emitter)
                (ecase v
                  (:square (// 'trial 'unit-square))
                  (:disc (// 'trial 'unit-disc))
                  (:sphere (// 'trial 'unit-sphere))
                  (:cube (// 'trial 'unit-cube)))))))
    (alloy:finish-structure panel layout focus)))
