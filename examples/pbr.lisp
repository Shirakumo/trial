(in-package #:org.shirakumo.fraf.trial.examples)

(define-asset (examples sphere) mesh
    (make-sphere-mesh 0.15 :segments 16))

(define-shader-entity pbr-globe (single-material-renderable transformed-entity)
  ((vertex-array :initform (// 'examples 'sphere))))

(define-example pbr
  :title "Physically Based Rendering"
  :description "Showcase of the Physically Based Rendering pipeline."
  (enter (make-instance 'editor-camera :location (vec 0.0 0.0 3) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'skybox :texture (assets:// :sandy-beach :environment-map)) scene)
  (enter (make-instance 'environment-light :asset (assets:asset :sandy-beach)) scene)

  (loop for (p c) in `((,(vec -10  10 10) ,(vec 300 300 300))
                       (,(vec  10  10 10) ,(vec 300 300 300))
                       (,(vec -10 -10 10) ,(vec 300 300 300))
                       (,(vec  10 -10 10) ,(vec 300 300 300)))
        do (enter (make-instance 'point-light :location p :color c) scene))
  (loop for y from 0.5 below 7
        for m = (/ (- y 0.5) 6.0)
        do (loop for x from 0.5 below 7
                 for r = (/ (- x 0.5) 6.0)
                 for p = (vec (* 2.5 (- r 0.5)) (* 2.5 (- m 0.5)) 0.0)
                 for mat = (make-instance 'pbr-material :albedo-texture (// 'trial 'white) :metal-rough-occlusion-texture (// 'trial 'white)
                                                        :albedo-factor (vec 0.5 0 0 1) :metalness-factor m :roughness-factor r :occlusion-factor 0.0)
                 do (enter (make-instance 'pbr-globe :location p :material mat) scene)
                 (print (list p m r))))
  
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'ward)))
    (connect (port render 'color) (port map 'previous-pass) scene)))
