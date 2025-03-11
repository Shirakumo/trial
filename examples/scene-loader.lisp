(in-package #:org.shirakumo.fraf.trial.examples)

(defclass debug-rigidbody-system (trial::debug-rigidbody-mixin
                                  accelerated-rigidbody-system)
  ())

(define-example scene-loader
  :title "Load Arbitrary Scenes"
  :description "Allows you to load arbitrary scenes from 3D files, including ones with physics simulations. By default shows the Sponza scene using a PBR renderer."
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'debug-rigidbody-system :units-per-metre 0.1))
          (incremental-load :initform NIL :accessor incremental-load)
          (file :initform NIL :accessor file)
          (paused-p :initform T :accessor paused-p))
  (enter (make-instance 'gravity :gravity (vec 0 -10 0)) scene)
  (let ((zpre (make-instance 'z-prepass))
        (render (make-instance 'ssao-pbr-render-pass :name :render :ssao-radius 1.0 :ssao-bias 0.1))
        (map (make-instance 'ward :name :map))
        (fxaa (make-instance 'fxaa-pass)))
    (connect (port zpre 'depth) (port render 'depth-map) scene)
    (connect (port render 'color) (port map 'previous-pass) scene)
    (connect (port map 'color) (port fxaa 'previous-pass) scene))
  (setf (file scene) (input* (assets:asset :sponza))))

(defmethod setup-ui ((scene scene-loader-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 140 140) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (let ((button (alloy:represent "Load File" 'alloy:button :focus-parent focus)))
      (alloy:enter button layout :row 0 :col 2)
      (alloy:on alloy:activate (button)
        (let ((file (org.shirakumo.file-select:existing :title "Load Model File..."
                                                        :filter '(("Model Files" "glb" "gltf" "obj")
                                                                  ("glTF" "glb" "gltf")
                                                                  ("Wavefront OBJ" "obj"))
                                                        :default (file scene))))
          (when file (setf (file scene) file)))))
    (alloy:finish-structure panel layout focus)))

(defmethod (setf file) :after (file (scene scene-loader-scene))
  (unless (incremental-load scene)
    (sequences:dosequence (entity scene)
      (when (typep entity '(and scene-node (not controller)))
        (leave entity T))))
  (setf (paused-p scene) T)
  (generate-resources 'model-file file :load-scene T)
  (unless (do-scene-graph (node scene)
            (when (typep node 'light) (return T)))
    (enter (make-instance 'skybox :texture (assets:// :sandy-beach :environment-map)) scene)
    (enter (make-instance 'environment-light :asset (assets:asset :sandy-beach) :color (vec3 0.3)) scene)
    (enter (make-instance 'directional-light :direction (nvunit (vec -0.2 -1 -0.1)) :color (vec3 10 10 8)) scene))
  (let ((camera (do-scene-graph (node scene)
                  (when (typep node 'camera) (return node)))))
    (if camera
        (setf (camera scene) camera)
        (ensure-entity :camera scene 'editor-camera
                       :location (vec3 10.0 20 14) :rotation (vec3 0.75 5.5 0.0) :fov 50 :move-speed 0.1)))
  (ensure-entity :grid scene 'vertex-entity :vertex-array (// 'trial 'grid))
  (commit scene (loader +main+)))

(define-handler ((scene scene-loader-scene) text-entered :after) (text)
  (case (char text 0)
    (#\p (setf (paused-p scene) (not (paused-p scene))))
    (#\r (setf (file scene) (file scene)))
    (#\c (setf (camera scene) (make-instance 'editor-camera :move-speed 0.1)))))

(define-handler ((scene scene-loader-scene) (ev trial::tick-event) :around) ()
  (cond ((paused-p scene)
         (handle ev (camera scene))
         (handle ev (node :controller scene))
         (loop for pass across (passes scene)
               do (handle ev pass))
         (project-view (camera scene)))
        (T
         (call-next-method))))
