(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity yukari (prefab basic-physics-entity)
  ())

(define-prefab-instantiation yukari (assets:pool assets::yukari)
  (<- :yukari))

(defmethod integrate :after ((yukari yukari) dt)
  (when (< (vy (location yukari)) -2)
    (leave yukari T)))

(define-example cereal
  :title "Cereal"
  :description "Crunchy cereal physics"
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'accelerated-rigidbody-system :units-per-metre 0.1)))
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'editor-camera :location (vec 0 1 3.6) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'skybox :texture (assets:// :sandy-beach :environment-map)) scene)
  (enter (make-instance 'environment-light :asset (assets:asset :sandy-beach) :color (vec3 0.3)) scene)
  (enter (make-instance 'directional-light :direction (nvunit (vec -0.2 -1 -0.1)) :color (vec3 10 10 8)) scene)
  (enter (make-instance 'gravity :gravity (vec 0 -10 0)) scene)
  
  (generate-resources 'model-file (assets:file :bowl) :load-scene T)
  (enter (make-instance 'yukari) scene)
  ;;(enter (make-instance 'basic-physics-entity :asset (assets:asset :bowl)) scene)
  
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'ward)))
    (connect (port render 'color) (port map 'previous-pass) scene)))

(define-handler (cereal-scene key-press :after) (key)
  (when (eql key :enter)
    (enter (make-prefab 'yukari :location (nv+ (vec 0 3 0) (vrand 0.0 5.0))
                                :orientation (qrand))
           cereal-scene)))


