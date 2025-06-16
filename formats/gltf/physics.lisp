(in-package #:org.shirakumo.fraf.trial.gltf)

(defun load-physics-geometry (geometry model &rest args)
  (let ((shape (gltf:shape geometry)))
    (etypecase shape
      (gltf:sphere-shape
       (vector (apply #'trial:make-sphere :radius (float (gltf:radius shape) 0f0) args)))
      (gltf:box-shape
       (vector (apply #'trial:make-box :bsize (nv* (to-vec (gltf:size shape)) 0.5) args)))
      (gltf:capsule-shape
       (vector (apply #'trial:make-pill :height (float (* 0.5 (gltf:height shape)) 0f0)
                                        :radius-bottom (float (gltf:radius-bottom shape) 0f0)
                                        :radius-top (float (gltf:radius-top shape) 0f0)
                                        args)))
      (gltf:cylinder-shape
       (vector (apply #'trial:make-cylinder
                      :height (float (* 0.5 (gltf:height shape)) 0f0)
                      :radius-top (float (gltf:radius-top shape) 0f0)
                      :radius-bottom (float (gltf:radius-bottom shape) 0f0)
                      args)))
      (null
       (let ((mesh (gltf:mesh (gltf:node geometry)))
             (local-transform (getf args :local-transform #.(meye 4))))
         (map 'vector (lambda (primitive)
                        (let* ((mesh (load-primitive primitive))
                               (verts (trial:reordered-vertex-data mesh '(trial:location)))
                               (faces (trial:faces mesh)))
                          (when (gltf:convex-p geometry)
                            ;; Blender's exported geo is FUCKED, and we don't trust it
                            ;; so we re-hull it here.
                            (multiple-value-setq (verts faces)
                              (org.shirakumo.fraf.quickhull:convex-hull verts)))
                          (apply (if (gltf:convex-p geometry)
                                     #'trial::make-maybe-optimized-convex-mesh
                                     #'trial:make-general-mesh)
                                 :vertices verts
                                 :faces (trial::simplify faces '(unsigned-byte 16))
                                 :local-transform (if (gltf:matrix primitive)
                                                      (m* local-transform (mat4 (gltf:matrix primitive)))
                                                      local-transform)
                                 args)))
              (gltf:primitives mesh)))))))

(defvar *physics-material-cache* (make-hash-table :test 'equal))
(defun physics-material-instance (material)
  (let ((name (list (gltf:static-friction material)
                    (gltf:dynamic-friction material)
                    (gltf:restitution material)
                    (gltf:friction-combine material)
                    (gltf:restitution-combine material))))
    (or (gethash name *physics-material-cache*)
        (setf (gethash name *physics-material-cache*)
              (trial:make-material-interaction-properties
               NIL NIL
               (gltf:static-friction material)
               (gltf:dynamic-friction material)
               (gltf:restitution material)
               (gltf:friction-combine material)
               (gltf:restitution-combine material))))))

(defun collision-filter-mask (filter)
  (flet ()
    (let ((mask (1- (ash 1 32))))
      (cond ((gltf:collide-with-systems filter)
             (setf mask 0)
             (loop for system across (gltf:collide-with-systems filter)
                   do (setf mask (logior mask (trial::collision-system-mask system)))))
            ((gltf:not-collide-with-systems filter)
             (loop for system across (gltf:collide-with-systems filter)
                   do (setf mask (logior mask (trial::collision-system-mask system))))))
      mask)))

(defun find-colliders (root-node child model)
  (let ((primitives (make-array 0 :adjustable T :fill-pointer T))
        (visited-p (make-hash-table :test 'eq))
        (material :wood)
        (mask 1))
    (labels ((process (node collider local-transform &optional animated)
               (when collider
                 (when (gltf:physics-material collider)
                   (setf material (physics-material-instance (gltf:physics-material collider))))
                 (when (gltf:collision-filter collider)
                   (setf mask (collision-filter-mask (gltf:collision-filter collider))))
                 (loop for primitive across (load-physics-geometry (gltf:geometry collider) model
                                                                   :local-transform (tmat local-transform)
                                                                   :collision-mask mask
                                                                   :material material)
                       do (vector-push-extend primitive primitives)
                          (when animated
                            (let* ((name (gltf-name node))
                                   (mesh (typecase (mesh child)
                                           (mesh-data (mesh child))
                                           (cons (first (mesh child)))
                                           (T (find-mesh (mesh child) model))))
                                   (joints (trial::joint-names (skeleton mesh))))
                              (setf (trial::primitive-joint-index primitive) (position name joints)))))))
             (recurse (node local-transform &optional animated)
               (unless (gethash node visited-p)
                 (setf (gethash node visited-p) T)
                 (let ((local-transform (t+ local-transform (gltf-node-transform node))))
                   (process node (gltf:collider node) local-transform animated)
                   (loop for child across (gltf:children node)
                         do (recurse child local-transform animated))))))
      (let ((local-transform (transform)))
        (process root-node (gltf:collider root-node) local-transform)
        (when (gltf:skin root-node)
          (loop for joint across (gltf:joints (gltf:skin root-node))
                do (loop for child across (gltf:children joint)
                         do (when (gltf:collider child)
                              (process joint (gltf:collider child) (gltf-node-transform* child root-node) T)))))
        (loop for child across (gltf:children root-node)
              do (recurse child local-transform)))
      (trial::simplify primitives))))

(defun load-rigidbody (model child node)
  ;; FIXME: implement joints
  (etypecase child
    (basic-entity (change-class child 'basic-physics-entity))
    (basic-animated-entity (change-class child 'animated-physics-entity)))
  (setf (trial:mass child) (gltf:mass (gltf:rigidbody node)))
  ;; FIXME: implement center-of-mass
  (when (/= 1.0 (gltf:gravity-factor (gltf:rigidbody node)))
    (v:warn :trial.gltf "Ignoring non-standard gravity factor on ~a" node))
  (let* ((r (gltf:inertia-orientation (gltf:rigidbody node)))
         (r (quat (aref r 0) (aref r 1) (aref r 2) (aref r 3)))
         (d (gltf:inertia-diagonal (gltf:rigidbody node))))
    (setf (trial:inertia-tensor child) (trial:diagonal-tensor d r)))
  (map-into (varr (trial:velocity child)) (lambda (x) (float x 0f0)) (gltf:linear-velocity (gltf:rigidbody node)))
  (map-into (varr (trial:rotation child)) (lambda (x) (float x 0f0)) (gltf:angular-velocity (gltf:rigidbody node)))
  ;; Extra support for damping factor
  (when (gltf:extras (gltf:rigidbody node))
    (setf (trial:damping child) (gethash "damping" (gltf:extras (gltf:rigidbody node)) 0.95)))
  ;; FIXME: this will eagerly decompose colliders and so on even if the node is never used...
  (setf (trial:physics-primitives child) (find-colliders node child model)))

(defvar *trigger-translator-functions* (make-hash-table :test 'equal))

(defmacro define-trigger-translation (gltf-class args &body body)
  `(setf (gethash ',gltf-class *trigger-translator-functions*)
         (lambda ,args
           ,@body)))

(defun load-trigger (model child node)
  (unless (typep child 'rigid-shape)
    (change-class child (typecase child
                          (basic-entity 'basic-physics-entity)
                          (animated-entity 'animated-physics-entity)
                          (T 'trigger-volume))))
  (setf (trial:physics-primitives child)
        (load-physics-geometry (gltf:geometry (gltf:trigger node)) model
                               :collision-mask (collision-filter-mask (gltf:collision-filter (gltf:trigger node)))))
  (with-simple-restart (continue "Ignore the trigger translation")
    (if (gltf:shirakumo-trigger-data node)
        (funcall (or (gethash (type-of (gltf:shirakumo-trigger-data node)) *trigger-translator-functions*)
                     (error "Unknown trigger volume type."))
                 child (gltf:shirakumo-trigger-data node))
        (funcall (gethash T *trigger-translator-functions*) child node))))

(defun %find-child (name node &optional errorp)
  (sequences:dosequence (child node (when errorp (error "No child named ~a found!" name)))
    (when (and (<= (length name) (length (name child)))
               (string= name (name child) :end2 (length name)))
      (return child))))

(define-trigger-translation T (trigger node)
  (declare (ignore node))
  (change-class trigger 'trial:trigger-volume))

(define-trigger-translation gltf:shirakumo-trigger (trigger trigger-data)
  (change-class trigger 'trial::simple-trigger-volume
                :type-expression (read-from-string (gltf:filter trigger-data))
                :form (gltf:form trigger-data)))

(define-trigger-translation gltf:shirakumo-spawner (trigger trigger-data)
  (destructuring-bind (&optional class-or-count &rest args) (enlist (read-from-string (gltf:spawn trigger-data)))
    (let ((spawn-volume (aref (physics-primitives trigger) 0))
          (trig-volume (%find-child "trigger" trigger))
          (spawn-count (gltf:spawn-count trigger-data)))
      (cond (trig-volume
             ;; Copy physics primitive //and transform// over
             (setf (physics-primitives trigger) (physics-primitives trig-volume))
             (!t+ (local-transform trigger) (local-transform trigger) (local-transform trig-volume)))
            (T
             (setf (physics-primitives trigger) (make-all-space))))
      (etypecase class-or-count
        (integer
         (setf spawn-count class-or-count)
         (setf class-or-count (%find-child "class" trigger T)))
        (symbol))
      (clear trigger)
      (change-class trigger 'trial::spawner-trigger-volume
                    :type-expression (read-from-string (gltf:filter trigger-data))
                    :spawn-class class-or-count
                    :spawn-arguments args
                    :spawn-count (or spawn-count 1)
                    :spawn-volume spawn-volume
                    :auto-deactivate (gltf:auto-deactivate-p trigger-data)
                    :snap-to-level (gltf:snap-to-surface-p trigger-data)
                    :respawn-cooldown (gltf:respawn-cooldown trigger-data)))))

(define-trigger-translation gltf:shirakumo-killvolume (trigger trigger-data)
  (change-class trigger 'trial::kill-trigger-volume
                :type-expression (read-from-string (gltf:filter trigger-data))
                :class-name (read-from-string (gltf:kill trigger-data))))

(define-trigger-translation gltf:shirakumo-checkpoint (trigger trigger-data)
  (let ((array (gltf:spawn-point trigger-data)))
    (change-class trigger 'trial::checkpoint-trigger
                  :type-expression (read-from-string (gltf:filter trigger-data))
                  :spawn-point (vec (aref array 0) (aref array 1) (aref array 2)))))

(define-trigger-translation gltf:shirakumo-progression (trigger trigger-data)
  (change-class trigger 'trial::global-sequence-trigger
                :condition (gltf:condition trigger-data)
                :sequence-id (gltf:state trigger-data)
                :new-value (gltf:value trigger-data)
                :modulation (ecase (gltf:mode trigger-data)
                              (:inc #'+)
                              (:dec #'-)
                              (:set (lambda (prev new)
                                      (declare (ignore prev))
                                      new)))))
