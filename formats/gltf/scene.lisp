(in-package #:org.shirakumo.fraf.trial.gltf)

(defun load-light (light)
  (flet ((make (type intensity &rest initargs)
           (apply #'make-instance type
                  :color (nv* (vec (aref (gltf:color light) 0)
                                   (aref (gltf:color light) 1)
                                   (aref (gltf:color light) 2))
                              intensity)
                  initargs)))
    (etypecase light
      (gltf:directional-light
       (make 'trial:directional-light
             (/ (sqrt (gltf:intensity light)) 100.0)
             :direction (vec 0 0 -1)))
      (gltf:point-light
       (make 'trial:point-light
             (/ (sqrt (gltf:intensity light)) 500.0)
             :linear-attenuation (or (gltf:range light) 0.0)
             :quadratic-attenuation 0.0))
      (gltf:spot-light
       (make 'trial:spot-light
             ;; FIXME: I have no funcking clue what I'm doing here
             (/ (sqrt (gltf:intensity light)) 500.0)
             :direction (vec 0 0 -1)
             :linear-attenuation (or (gltf:range light) 0.0)
             :inner-radius (rad->deg (gltf:inner-angle light))
             :outer-radius (rad->deg (gltf:outer-angle light)))))))

(defun load-environment-light (light)
  (let ((envmap (trial:implement!)))
    (list (make-instance 'trial:environment-light
                         :color (vec (gltf:intensity light) (gltf:intensity light) (gltf:intensity light))
                         :irradiance-map (trial:implement!)
                         :environment-map envmap)
          (make-instance 'trial:skybox :texture (resource envmap :environment-map)))))

(defun load-camera (camera)
  (etypecase camera
    (gltf:orthographic-camera
     (make-instance 'trial:2d-camera :near-plane (float (gltf:znear camera) 0f0)
                                     :far-plane (float (gltf:zfar camera) 0f0)
                                     :location (vec 0 0 0)))
    (gltf:perspective-camera
     (make-instance 'trial:target-camera :fov (float (rad->deg (gltf:fov camera)) 0f0)
                                         :near-plane (float (gltf:znear camera) 0f0)
                                         :far-plane (float (gltf:zfar camera) 0f0)
                                         :location (vec 0 0 0)
                                         :target (vec 0 0 -1)))))

(defmethod load-model (input (type (eql :glb)) &rest args)
  (apply #'load-model input :gltf args))

(defun %construct-node (node gltf meshes generator)
  (cond ((loop for skin across (gltf:skins gltf)
               thereis (loop for joint across (gltf:joints skin)
                             thereis (eq joint node)))
         ;; Eliminate nodes that are parts of a skin
         NIL)
        ((gltf:virtual-p node)
         ;; Eliminate nodes that are marked as virtual
         NIL)
        ((gltf:mesh node)
         (let ((mesh-name (gltf-name (gltf:mesh node))))
           (make-instance (etypecase (or (gethash mesh-name meshes)
                                         (gethash (cons mesh-name 0) meshes))
                            (static-mesh 'basic-entity)
                            ;; FIXME: instead of turning each skin into an animated entity
                            ;;        we should share the pose between them and only make one
                            ;;        animated entity the controller
                            (animated-mesh 'basic-animated-entity))
                          :lods (loop for i from -1
                                      for threshold across (gltf:lod-screen-coverage node)
                                      for lod = mesh-name then (gltf-name (gltf:mesh (aref (gltf:lods node) i)))
                                      collect (make-instance 'lod :threshold threshold :mesh lod))
                          :transform (gltf-node-transform node)
                          :name (gltf-name node)
                          :asset generator
                          :mesh mesh-name)))
        (T
         (make-instance 'basic-node :transform (gltf-node-transform node)
                                    :name (gltf-name node)))))

(defmethod translate-node (node entity gltf)
  entity)

(defun construct-node (node gltf model generator)
  (let ((entity (%construct-node node (meshes model) generator gltf)))
    (when entity
      (loop for child across (gltf:children node)
            for child-entity = (construct-node child gltf model generator)
            do (when child-entity (enter child-entity entity)))
      (when (gltf:light node)
        (enter (load-light (gltf:light node)) entity))
      (when (gltf:camera node)
        (enter (load-camera (gltf:camera node)) entity))
      (when (and (not (gltf:trigger node)) (gltf:rigidbody node))
        (load-rigidbody model entity node))
      (when (gltf:trigger node)
        (load-trigger model entity node))
      (translate-node node entity gltf))))

(defmethod load-model (input (type (eql :gltf)) &key (generator (make-instance 'resource-generator))
                                                     (model (make-instance 'model)))
  (gltf:with-gltf (gltf input)
    (let ((meshes (meshes model))
          (clips (clips model))
          (scenes (scenes model)))
      (load-materials gltf model generator)
      (loop for mesh across (load-meshes gltf model)
            do (setf (gethash (name mesh) meshes) mesh)
               (trial::make-vertex-array mesh (resource generator (name mesh))))
      ;; Patch up
      (load-clips gltf clips)
      (when (loop for mesh being the hash-values of meshes
                  thereis (skinned-p mesh))
        (setf (skeleton model) (load-skeleton gltf))
        (let ((map (make-hash-table :test 'eql)))
          (trial::reorder (skeleton model) map)
          (loop for clip being the hash-values of clips
                do (trial::reorder clip map))
          (loop for mesh being the hash-values of meshes
                do (when (skinned-p mesh)
                     (trial::reorder mesh map)))))
      ;; Construct scene graphs
      (loop for node across (gltf:scenes gltf)
            for scene = (make-instance 'basic-node :name (gltf-name node))
            do (setf (gethash (name scene) scenes) scene)
               (when (gltf:light node)
                 (dolist (object (load-environment-light (gltf:light node)))
                   (enter object scene)))
               (when (gltf:envmap node)
                 (let ((envmap (make-instance 'environment-map :input (merge-pathnames (gltf:envmap node) input))))
                   (enter (make-instance 'environment-light :asset envmap :name :envlight) scene)
                   (enter (make-instance 'skybox :texture (resource envmap :environment-map) :name :skybox) scene)))
               (loop for child across (gltf:nodes node)
                     for entity = (construct-node child gltf model generator)
                     do (when entity (enter entity scene))))
      model)))

