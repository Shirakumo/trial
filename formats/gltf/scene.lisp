(in-package #:org.shirakumo.fraf.trial.gltf)

(defun convert-light-intensity (i)
  (let ((watts (float (/ i 54.35141306588226d0) 0f0)))
    (/ watts 80.0)))

(defun load-light (light)
  (flet ((make (type intensity &rest initargs)
           (apply #'make-instance type
                  :color (nv* (to-vec (gltf:color light)) intensity)
                  initargs)))
    (etypecase light
      (gltf:directional-light
       (make 'trial:directional-light
             (convert-light-intensity (gltf:intensity light))
             :direction (vec 0 0 -1)))
      (gltf:point-light
       (make 'trial:point-light
             (convert-light-intensity (gltf:intensity light))
             :linear-attenuation (or (gltf:range light) 1.0)
             :quadratic-attenuation 0.0))
      (gltf:spot-light
       (make 'trial:spot-light
             (convert-light-intensity (gltf:intensity light))
             :direction (vec 0 0 -1)
             :linear-attenuation (or (gltf:range light) 1.0)
             :quadratic-attenuation 0.0
             :inner-radius (rad->deg (gltf:inner-angle light))
             :outer-radius (rad->deg (gltf:outer-angle light)))))))

(defun load-environment-light (light)
  (let ((envmap (trial:implement!)))
    (list (make-instance 'trial:environment-light
                         :color (vec3 (gltf:intensity light))
                         :irradiance-map (trial:implement!)
                         :environment-map envmap)
          (make-instance 'trial:skybox :texture (resource envmap :environment-map)))))

(defun load-camera (camera)
  (etypecase camera
    (gltf:orthographic-camera
     (make-instance 'trial:2d-camera :near-plane (float (gltf:znear camera) 0f0)
                                     :far-plane (float (gltf:zfar camera) 0f0)
                                     :location (vec3 0)))
    (gltf:perspective-camera
     (make-instance 'trial:target-camera :fov (float (rad->deg (gltf:fov camera)) 0f0)
                                         :near-plane (float (gltf:znear camera) 0f0)
                                         :far-plane (float (gltf:zfar camera) 0f0)
                                         :location (vec3 0)
                                         :target (vec 0 0 -1)))))

(defun %construct-node (node gltf meshes generator)
  (cond ((loop for skin across (gltf:skins gltf)
               thereis (find node (gltf:joints skin)))
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
                            (animated-mesh 'basic-animated-entity))
                          :lods (loop for i from -1
                                      for threshold across (gltf:lod-screen-coverage node)
                                      for lod = mesh-name then (gltf-name (gltf:mesh (aref (gltf:lods node) i)))
                                      collect (make-instance 'lod :threshold threshold :mesh lod))
                          :transform (gltf-node-transform node)
                          :name (gltf-name node)
                          :asset generator
                          :mesh (trial:find-meshes mesh-name meshes))))
        (T
         (make-instance 'basic-node :transform (gltf-node-transform node)
                                    :name (gltf-name node)))))

(defmethod translate-node (node entity gltf)
  (let ((found NIL))
    ;; KLUDGE: alias animation controllers shared between children
    ;;         by turning the current node into a controller instead.
    (do-scene-graph (child entity)
      (unless (eq child entity)
        (when (typep child 'animated-entity)
          (setf found child)
          (setf (animation-controller child) entity))))
    (when found
      (change-class entity 'basic-animation-controller)
      (unless (eq entity (animation-controller found))
        (<- entity (animation-controller found))))
    (when (and (gltf:instance-of node)
               (string/= "" (gltf:instance-of node)))
      (destructuring-bind (class . args) (enlist (read-from-string (gltf:instance-of node)))
        (apply #'change-class entity class args))))
  entity)

(defun construct-node (node gltf model generator)
  (let ((entity (%construct-node node gltf (meshes model) generator)))
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

(defun %load-model (input &key (generator (make-instance 'resource-generator))
                               (model (make-instance 'model)))
  (gltf:with-gltf (gltf input)
    (let ((meshes (meshes model))
          (scenes (scenes model)))
      (when (pathnamep input)
        (setf (name model) (pathname-name input)))
      (load-materials gltf model generator)
      (load-clips gltf NIL (clips model))
      (loop for mesh across (load-meshes gltf :model model)
            do (setf (gethash (name mesh) meshes) mesh)
               (make-vertex-array mesh (resource generator (name mesh))))
      ;; Construct scene graphs
      (loop for node across (gltf:scenes gltf)
            for scene = (make-instance 'basic-node :name (gltf-name node))
            do (setf (gethash (name scene) scenes) scene)
               (when (gltf:light node)
                 (dolist (object (load-environment-light (gltf:light node)))
                   (enter object scene)))
               (when (gltf:envmap node)
                 (let* ((envmap (gltf:envmap node))
                        (color (to-vec (gltf:color envmap)))
                        (orientation (to-vec (gltf:orientation envmap)))
                        (envmap (make-instance 'environment-map :input (merge-pathnames (gltf:file envmap) input))))
                   (enter (make-instance 'environment-light :color color :asset envmap :name :envlight) scene)
                   (enter (make-instance 'skybox :color color :texture (resource envmap :environment-map) :name :skybox) scene)))
               (loop for child across (gltf:nodes node)
                     for entity = (construct-node child gltf model generator)
                     do (when entity (enter entity scene)))
               ;; Check whether the scene contains any animated nodes, enter a controller if so.
               (let ((clip-targets (loop for clip being the hash-values of (clips model)
                                         nconc (loop for track across (tracks clip)
                                                     when (symbolp (name track))
                                                     collect (name track)
                                                     when (consp (name track))
                                                     collect (car (name track))))))
                 (do-scene-graph (node scene)
                   (when (member (name node) clip-targets)
                     (enter (make-instance 'scene-animation-controller :clips (clips model)) scene)))))
      model)))

;; Special case override so we still get support for general converters
;; such as for depot entries and transactions.
(defmethod load-model ((input stream) (type (eql :gltf)) &rest args &key &allow-other-keys)
  (apply #'%load-model input args))

(defmethod load-model ((input pathname) (type (eql :gltf)) &rest args &key &allow-other-keys)
  (apply #'%load-model input args))

(defmethod load-model ((input vector) (type (eql :gltf)) &rest args &key &allow-other-keys)
  (apply #'%load-model input args))

(defmethod load-model (input (type (eql :gltf)) &rest args &key &allow-other-keys)
  (typecase input
    (cffi:foreign-pointer
     (apply #'%load-model input args))
    (T (call-next-method))))

(defmethod load-model :around (input (type (eql :glb)) &rest args)
  (apply #'load-model input :gltf args))
