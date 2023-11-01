(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity decomposition-entity (vertex-entity colored-entity transformed-entity)
  ((visible-p :initarg :visible-p :initform T :accessor visible-p)))

(defmethod render :around ((entity decomposition-entity) (program shader-program))
  (when (visible-p entity)
    (gl:polygon-mode :front-and-back (polygon-mode (scene +main+)))
    (call-next-method)
    (gl:polygon-mode :front-and-back :fill)))

(defclass mesh-item (alloy:combo-item) ())
(defmethod alloy:text ((item mesh-item)) (name (alloy:value item)))

(defmethod alloy:combo-item ((item mesh-data) (combo alloy:combo))
  (make-instance 'mesh-item :value item))

(define-example decomposition
  :title "Convex Hull Decomposition"
  :superclasses (alloy:observable)
  :slots ((model :initform NIL :accessor model)
          (mesh :initform NIL :accessor mesh)
          (polygon-mode :initform :fill :accessor polygon-mode)
          (show-original :initform NIL :accessor show-original)
          (file :initform NIL :accessor file))
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 10) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'basic-node :name :container) scene))

(alloy:define-observable (setf model) (value alloy:observable))
(alloy:define-observable (setf mesh) (value alloy:observable))

(defmethod setup-ui ((scene decomposition-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(120 140 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (alloy:enter "Load Model" layout :row 0 :col 0)
    (let ((button (alloy:represent "..." 'alloy:button :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:activate (button)
        (let ((file (org.shirakumo.file-select:existing :title "Load Model File..."
                                                        :filter '(("Wavefront OBJ" "obj")
                                                                  ("glTF File" "gltf")
                                                                  ("glTF Binary" "glb"))
                                                        :default (file panel))))
          (when file (setf (file panel) file)))))
    (alloy:enter "Mesh" layout :row 1 :col 0)
    (let ((selector (alloy:represent (mesh scene) 'alloy:combo-set :value-set () :layout-parent layout :focus-parent focus)))
      (alloy:on model (model scene)
        (let ((meshes (if (typep model 'model) (list-meshes model) ())))
          (setf (alloy:value-set selector) meshes)
          (when meshes (setf (alloy:value selector) (first meshes)))))
      (alloy:on alloy:value (mesh selector)
        (setf (mesh scene) mesh)))
    (alloy:enter "Show Original" layout :row 2 :col 0)
    (alloy:represent (show-original scene) 'alloy:switch :layout-parent layout :focus-parent focus)
    (alloy:enter "Wireframe" layout :row 3 :col 0)
    (alloy:represent (polygon-mode scene) 'alloy:switch :layout-parent layout :focus-parent focus
                                                        :on :line :off :fill)
    (load (assets:asset :woman))
    (setf (model scene) (assets:asset :woman))
    (alloy:finish-structure panel layout focus)))

(defmethod (setf file) :before (file (scene decomposition-scene))
  (setf (model scene) (generate-resources 'model-loader file)))

(defmethod (setf show-original) :after (value (scene decomposition-scene))
  (let ((orig (node :original (container scene))))
    (when orig (setf (visible-p orig) value))))

(defmethod (setf mesh) :before ((mesh mesh-data) (scene decomposition-scene))
  (clear (node :container scene))
  (enter (make-instance 'decomposition-entity
                        :name :original
                        :color (vec 1 1 1 0.5)
                        :visible-p (show-original scene)
                        :vertex-array (make-vertex-array 
                                       (make-convex-mesh
                                        :vertices (reordered-vertex-data mesh '(location))
                                        :faces (trial::simplify (index-data mesh) '(unsigned-byte 32)))
                                       NIL))
         (node :container scene))
  (loop for hull across (org.shirakumo.fraf.convex-covering:decompose
                         (reordered-vertex-data mesh '(location))
                         (trial::simplify (index-data mesh) '(unsigned-byte 32)))
        for (name . color) in (apply #'alexandria:circular-list (colored:list-colors))
        do (enter (make-instance 'decomposition-entity
                                 :color (vec (colored:r color) (colored:g color) (colored:b color))
                                 :vertex-array (make-vertex-array (make-convex-mesh :vertices (org.shirakumo.fraf.convex-covering:vertices hull)
                                                                                    :faces (org.shirakumo.fraf.convex-covering:faces hull))
                                                                  NIL))
                  (node :container scene)))
  (commit (scene +main+) (loader +main+)))
