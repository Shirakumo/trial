(in-package #:org.shirakumo.fraf.trial.examples)

(defclass debug-rigidbody-system (trial::debug-rigidbody-mixin
                                  accelerated-rigidbody-system)
  ())

(define-example scene-loader
  :title "Load Arbitrary Scenes"
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'accelerated-rigidbody-system :units-per-metre 0.1))
          (incremental-load :initform NIL :accessor incremental-load)
          (file :initform NIL :accessor file)
          (paused-p :initform T :accessor paused-p)
          (collision-debug-p :initform NIL :accessor collision-debug-p)
          (include-fixed-p :initform NIL :accessor include-fixed-p))
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 10.0 20 14) :rotation (vec3 0.75 5.5 0.0) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'directional-light :direction -vy3+) scene)
  (enter (make-instance 'ambient-light :color (vec3 0.5)) scene)
  (enter (make-instance 'gravity :gravity (vec 0 -10 0)) scene)
  (observe! (paused-p scene) :title "Pause [P]")
  (let ((render (make-instance 'pbr-render-pass))
        (map (make-instance 'ward)))
    (connect (port render 'color) (port map 'previous-pass) scene))
  (setf (file scene) (input* (assets:asset :physics-test))))

(defmethod setup-ui ((scene scene-loader-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 140 140) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (let ((button (alloy:represent "Load File" 'alloy:button :focus-parent focus)))
      (alloy:enter button layout :row 0 :col 2)
      (alloy:on alloy:activate (button)
        (let ((file (org.shirakumo.file-select:existing :title "Load Model File..."
                                                        :filter '(("glTF" "glb" "gltf"))
                                                        :default (file scene))))
          (when file (setf (file scene) file))))

      (alloy:enter "Collision Debug" layout :row 1 :col 1)
      (let ((debug (alloy:represent-with T (make-instance 'alloy:accessor-data :object scene :accessor 'collision-debug-p) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value debug)
          (if value
              (change-class (physics-system scene) 'debug-rigidbody-system)
              (change-class (physics-system scene) 'accelerated-rigidbody-system))))

      (alloy:enter "Include Fixed" layout :row 2 :col 1)
      (let ((fixed (alloy:represent-with 'alloy:combo-set (make-instance 'alloy:accessor-data :object scene :accessor 'include-fixed-p)
                                         :value-set '(NIL :mixed T) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value fixed)
          (let ((physics-system (physics-system scene)))
            (when (typep physics-system 'debug-rigidbody-system)
              (setf (trial::include-fixed-p physics-system) value))))))

    (alloy:finish-structure panel layout focus)))

(defmethod (setf file) :after (file (scene scene-loader-scene))
  (unless (incremental-load scene)
    (for:for ((entity over scene))
      (when (typep entity 'basic-node)
        (leave entity T))))
  (setf (paused-p scene) T)
  (generate-resources 'model-file file :load-scene T)
  ;; FIXME: auto-fit camera to model
  (commit scene (loader +main+)))

(define-handler ((scene scene-loader-scene) text-entered :after) (text)
  (case (char text 0)
    (#\p (setf (paused-p scene) (not (paused-p scene))))))

(define-handler ((scene scene-loader-scene) (ev tick) :around) ()
  (cond ((paused-p scene)
         (handle ev (camera scene))
         (handle ev (node :controller scene)))
        (T
         (call-next-method))))
