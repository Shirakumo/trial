(in-package #:org.shirakumo.fraf.trial.examples)

(define-example logo
  :title "Logo Animations"
  :description "Shows the engine and team logo animations"
  (disable-feature :cull-face)
  (enter (make-instance 'ambient-light :color (vec3 2)) scene)
  (enter (make-instance 'basic-animated-entity :name :logo :asset (assets:asset :shirakumo)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 400) scene)
  (enter (make-instance 'phong-render-pass :clear-color (vec4 0)) scene))

(defmethod change-scene :after (main (scene logo-scene) &key)
  ;; The clip is not attached to a specific mesh in the asset, but rather stored in the model,
  ;; as a global scene clip, so we have to retrieve and play it like this.
  (play (find-clip :logo.svg.002 (assets:asset :shirakumo)) (node :logo scene)))
