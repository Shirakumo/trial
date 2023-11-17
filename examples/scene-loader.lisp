(in-package #:org.shirakumo.fraf.trial.examples)

(define-example scene-loader
  :title "Load Arbitrary Scenes"
  :superclasses (trial:physics-scene)
  :slots ((physics-system :initform (make-instance 'accelerated-rigidbody-system :units-per-metre 0.1))
          (incremental-load :initform NIL :accessor incremental-load)
          (file :initform NIL :accessor file)
          (paused-p :initform T :accessor paused-p))
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
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 140) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (let ((button (alloy:represent "Load File" 'alloy:button :focus-parent focus)))
      (alloy:enter button layout :row 0 :col 1)
      (alloy:on alloy:activate (button)
        (let ((file (org.shirakumo.file-select:existing :title "Load Model File..."
                                                        :filter '(("glTF Binary" "glb")
                                                                  ("glTF File" "gltf"))
                                                        :default (file scene))))
          (when file (setf (file scene) file)))))
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
