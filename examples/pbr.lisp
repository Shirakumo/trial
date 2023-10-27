(in-package #:org.shirakumo.fraf.trial.examples)

(define-example pbr
  :title "Physically Based Rendering"
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'skybox :texture (assets:// :sandy-beach :environment-map)) scene)
  (enter (make-instance 'environment-light :asset (assets:asset :sandy-beach) :color (vec 0.3 0.3 0.3)) scene)
  (generate-resources (assets:asset :marble-bust) T :load-scene T)
  (setf (scaling (node "marble_bust_01" scene)) 10)
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'tone-mapping-pass)))
    (connect (port render 'color) (port map 'previous-pass) scene)))
