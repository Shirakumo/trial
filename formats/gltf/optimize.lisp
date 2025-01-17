(in-package #:org.shirakumo.fraf.trial.gltf)

(defun add-convex-mesh (gltf primitives &optional name)
  (flet ((make-primitive (geometry)
           (gltf:make-mesh-primitive gltf (car geometry) (cdr geometry) '(:position))))
    (gltf:make-indexed 'gltf:mesh gltf
                       :name name
                       :primitives (map 'vector #'make-primitive primitives))))

(defmethod optimize-model (file (type (eql :glb)) &rest args)
  (apply #'optimize-model file :gltf args))

(defmethod optimize-model (file (type (eql :gltf)) &rest args &key (output file) &allow-other-keys)
  (let ((decomposition-args (remf* args :output))
        (mesh-table (make-hash-table :test 'eql))
        (work-done-p NIL))
    (trial:with-tempfile (tmp :type (pathname-type file))
      (gltf:with-gltf (gltf file)
        ;; Rewrite mesh shapes to multiple new shapes.
        ;; TODO: if original mesh has no other refs anywhere, remove it
        (loop for node across (gltf:nodes gltf)
              for collider = (gltf:collider node)
              do (when collider 
                   (let* ((collider (gltf:collider node))
                          (geometry (gltf:geometry collider)))
                     (when (gltf:node geometry)
                       (let ((new (gethash (gltf:mesh (gltf:node geometry)) mesh-table)))
                         (unless new
                           (let* ((node (gltf:node geometry))
                                  (mesh (gltf:mesh node))
                                  (meshes (loop for primitive across (gltf:primitives mesh)
                                                for mesh = (load-primitive primitive)
                                                collect (cons (reordered-vertex-data mesh '(location)) (faces mesh))))
                                  (meshes (cond ((gltf:convex-p geometry)
                                                 (v:info :trial.gltf "Re-hulling ~a" mesh)
                                                 (loop for (vertices) in meshes
                                                       collect (multiple-value-bind (vertices faces) (org.shirakumo.fraf.quickhull:convex-hull vertices)
                                                                 (cons vertices faces))))
                                                (T
                                                 (v:info :trial.gltf "Decomposing ~a" mesh)
                                                 (loop for (vertices . faces) in meshes
                                                       nconc (coerce (apply #'trial::decompose-to-convex vertices faces decomposition-args) 'list)))))
                                  (new-mesh (add-convex-mesh gltf meshes (format NIL "~a/~:[decomposed~;rehulled~]"
                                                                                 (gltf:name mesh) (gltf:convex-p geometry)))))
                             (v:info :trial.gltf "Creating new mesh ~a" new-mesh)
                             (setf new (gltf:make-indexed 'gltf:node node :mesh new-mesh
                                                                          :name (gltf:name new-mesh)
                                                                          :virtual-p T
                                                                          :matrix (gltf:matrix node)
                                                                          :rotation (gltf:rotation node)
                                                                          :scale (gltf:scale node)
                                                                          :translation (gltf:translation node)))
                             (setf (gethash mesh mesh-table) new)))
                         (v:info :trial.gltf "Updating ~a to point to ~a" node new)
                         (setf (gltf:node geometry) new)
                         (setf (gltf:convex-p geometry) T)
                         (when (gltf:extensions node)
                           (remhash "KHR_physics_rigid_bodies" (gltf:extensions node)))
                         (setf work-done-p T))))))
        (when work-done-p
          (gltf:serialize gltf tmp)))
      (when (and work-done-p output)
        ;; FIXME: this does not work correctly if gltf serialises to multiple files.
        (org.shirakumo.filesystem-utils:rename-file* tmp output)))))
