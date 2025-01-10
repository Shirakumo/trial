(in-package #:org.shirakumo.fraf.trial.gltf)

(defun add-convex-shape (gltf vertices faces)
  (let* ((primitive (gltf:make-mesh-primitive gltf vertices faces '(:position)))
         (mesh (gltf:make-indexed 'gltf:mesh gltf :primitives (vector primitive))))
    (gltf:make-indexed 'gltf:mesh-shape gltf :mesh mesh :kind "mesh" :convex-p T)))

(defun push-convex-shape (base-node shape)
  (let* ((collider (make-instance 'gltf:collider :collision-filter (gltf:collision-filter (gltf:collider base-node))
                                                 :physics-material (gltf:physics-material (gltf:collider base-node))
                                                 :shape shape
                                                 :gltf (gltf:gltf base-node)))
         (child (gltf:make-indexed 'gltf:node base-node :collider collider :virtual-p T)))
    (gltf:push-child child base-node)))

(defmethod optimize-model (file (type (eql :glb)) &rest args)
  (apply #'optimize-model file :gltf args))

(defun shape-optimizable-p (shape)
  (and (typep shape 'gltf:mesh-shape)
       (not (gltf:convex-p shape))))

(defmethod optimize-model (file (type (eql :gltf)) &rest args &key (output file) &allow-other-keys)
  (let ((decomposition-args (remf* args :output))
        (shape-table (make-hash-table :test 'eql))
        (work-done-p NIL))
    (trial:with-tempfile (tmp :type (pathname-type file))
      (gltf:with-gltf (gltf file)
        ;; Rewrite mesh shapes to multiple new shapes.
        ;; TODO: if original mesh has no other refs anywhere, remove it
        (loop for shape across (gltf:shapes gltf)
              do (when (and (shape-optimizable-p shape)
                            ;; Only bother decomposing it if it's actually referenced anywhere.
                            (loop for node across (gltf:nodes gltf)
                                  thereis (and (gltf:collider node) (eql shape (gltf:shape (gltf:collider node))))))
                   (let* ((primitives (gltf:primitives (gltf:mesh shape)))
                          (mesh (load-primitive (aref primitives 0)))
                          (verts (reordered-vertex-data mesh '(location)))
                          (hulls (apply #'trial::decompose-to-convex verts (faces mesh) decomposition-args)))
                     (setf (gethash shape shape-table)
                           (loop for (verts . faces) across hulls
                                 collect (add-convex-shape gltf verts faces))))))
        ;; Rewrite nodes with refs to mesh colliders to have child nodes for
        ;; all decomposed hulls.
        (loop for node across (gltf:nodes gltf)
              do (when (and (gltf:collider node) (shape-optimizable-p (gltf:shape (gltf:collider node))))
                   (loop for shape in (gethash (gltf:shape (gltf:collider node)) shape-table)
                         do (push-convex-shape node shape))
                   (setf (gltf:collider node) NIL)
                   ;; Clear the extension, too. Ideally this would be done by the library already.
                   (remhash "collider" (gethash "KHR_physics_rigid_bodies" (gltf:extensions node)))
                   (setf work-done-p T)))
        (when work-done-p
          (gltf:serialize gltf tmp)))
      (when work-done-p
        ;; FIXME: this does not work correctly if gltf serialises to multiple files.
        (org.shirakumo.filesystem-utils:rename-file* tmp output)))))
