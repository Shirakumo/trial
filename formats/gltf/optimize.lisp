(in-package #:org.shirakumo.fraf.trial.gltf)

(defun add-shapes-mesh (gltf shapes &optional name)
  (flet ((make-primitive (shape)
           (gltf:make-mesh-primitive
            gltf
            (convex-mesh-vertices shape)
            (convex-mesh-faces shape)
            '(:position)
            :matrix (marr4 (primitive-local-transform shape)))))
    (gltf:make-indexed 'gltf:mesh gltf
                       :name name
                       :primitives (map 'vector #'make-primitive shapes))))

(defun optimized-p (mesh)
  (and (gltf:name mesh)
       (cl-ppcre:scan "/(decomposed|rehulled)$" (gltf:name mesh))))

(defun optimize-geometry (gltf geometry)
  (let* ((node (gltf:node geometry))
         (shapes (load-physics-geometry geometry NIL))
         (shapes (trial::convexify shapes :rehull T))
         (mesh (add-shapes-mesh gltf shapes (format NIL "~a/~:[decomposed~;rehulled~]"
                                                    (gltf:name (gltf:mesh node))
                                                    (gltf:convex-p geometry)))))
    (v:info :trial.gltf "Creating new mesh ~a" mesh)
    (gltf:make-indexed 'gltf:node node
                       :mesh mesh
                       :name (gltf:name mesh)
                       :virtual-p T
                       :matrix (gltf:matrix node)
                       :rotation (gltf:rotation node)
                       :scale (gltf:scale node)
                       :translation (gltf:translation node))))

(defmethod optimize-model (file (type (eql :gltf)) &key (output #p""))
  (let ((mesh-table (make-hash-table :test 'eql))
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
                     (when (and (gltf:node geometry)
                                (not (optimized-p (gltf:mesh (gltf:node geometry)))))
                       (let ((new (or (gethash (gltf:mesh (gltf:node geometry)) mesh-table)
                                      (setf (gethash (gltf:mesh (gltf:node geometry)) mesh-table)
                                            (optimize-geometry gltf geometry)))))
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
        (org.shirakumo.filesystem-utils:rename-file* tmp (merge-pathnames output file))))))

(defmethod optimize-model (file (type (eql :glb)) &rest args &key &allow-other-keys)
  (apply #'optimize-model file :gltf args))
