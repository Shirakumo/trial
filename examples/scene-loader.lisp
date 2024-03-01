(in-package #:org.shirakumo.fraf.trial.examples)

(defclass debug-rigidbody-system (trial::debug-rigidbody-mixin
                                  accelerated-rigidbody-system)
  ())

(define-example scene-loader
  :title "Load Arbitrary Scenes"
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'debug-rigidbody-system :units-per-metre 0.1))
          (incremental-load :initform NIL :accessor incremental-load)
          (file :initform NIL :accessor file)
          (paused-p :initform T :accessor paused-p))
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'gravity :gravity (vec 0 -10 0)) scene)
  (observe! (paused-p scene) :title "Pause [P]")
  (let ((render (make-instance 'pbr-render-pass :name :render))
        (map (make-instance 'ward :name :map)))
    (connect (port render 'color) (port map 'previous-pass) scene))
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
          (when file (setf (file scene) file))))

      (alloy:enter "Collision Debug" layout :row 1 :col 1)
      (alloy:represent (trial::show-collision-debug (physics-system scene)) 'alloy:switch
                       :layout-parent layout :focus-parent focus)

      (alloy:enter "Include Fixed" layout :row 2 :col 1)
      (alloy:represent (trial::include-fixed-p (physics-system scene)) 'alloy:combo-set
                       :value-set '(NIL :mixed T) :layout-parent layout :focus-parent focus))

    (alloy:finish-structure panel layout focus)))

(defmethod (setf file) :after (file (scene scene-loader-scene))
  (unless (incremental-load scene)
    (sequence:dosequence (entity scene)
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
        (enter (make-instance 'editor-camera :name :camera :location (VEC3 10.0 20 14) :rotation (vec3 0.75 5.5 0.0) :fov 50 :move-speed 0.1) scene)))
  (enter (make-instance 'vertex-entity :name :grid :vertex-array (// 'trial 'grid)) scene)
  (commit scene (loader +main+)))

(define-handler ((scene scene-loader-scene) text-entered :after) (text)
  (case (char text 0)
    (#\p (setf (paused-p scene) (not (paused-p scene))))
    (#\r (setf (file scene) (file scene)))))

(define-handler ((scene scene-loader-scene) (ev tick) :around) ()
  (cond ((paused-p scene)
         (handle ev (camera scene))
         (handle ev (node :controller scene)))
        (T
         (call-next-method))))
