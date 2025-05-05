(in-package #:org.shirakumo.fraf.trial)

(defclass rigid-shape (physics-entity transformed-entity global-bounds-cached-entity)
  ((physics-primitives :initform #() :accessor physics-primitives)
   ;; Cache
   (transform-matrix :initform (mat4) :reader transform-matrix)
   (collision-mask :initform (1- (ash 1 32)) :accessor collision-mask)))

(defmethod shared-initialize :after ((body rigid-shape) slots &key physics-primitives)
  (when physics-primitives (setf (physics-primitives body) physics-primitives)))

(defmethod describe-object :after ((shape rigid-shape) stream)
  (format stream "~&~%Collision Systems:~{~%  ~a~}"
          (collision-mask-systems (collision-mask shape)))
  (format stream "~&~%Local Transform:~%")
  (write-transform (local-transform shape) stream)
  (format stream "~&~%Global Transform:~%")
  (write-transform (global-transform-matrix shape) stream)
  (format stream "~&~%Physics Primitives (~d):" (length (physics-primitives shape)))
  (loop for primitive across (physics-primitives shape)
        do (format stream "~%  ~a~%" primitive)
           (format stream "~4ton system~p:~{ ~a~}~%"
                   (length (collision-mask-systems (collision-mask primitive)))
                   (collision-mask-systems (collision-mask primitive)))
           (when (<= 0 (primitive-joint-index primitive))
             (format stream "~4tanimated by: joint ~d (~s)"
                     (primitive-joint-index primitive)
                     (if (or (skinned-p shape) (typep shape 'skeleton-controller))
                         (elt (joint-names (skeleton shape)) (primitive-joint-index primitive))
                         "ERROR: NOT SKINNED?")))))

(define-transfer rigid-shape
  (physics-primitives physics-primitives (lambda (p) (map-into (make-array (length p)) #'clone p))))

(define-hit-detector (rigid-shape T)
  (loop for ai across (physics-primitives a)
        do (detect-hits ai b)))

(define-hit-detector (rigid-shape rigid-shape)
  (loop for ai across (physics-primitives a)
        do (loop for bi across (physics-primitives b)
                 do (detect-hits ai b))))

(define-distance (rigid-shape T)
  (loop for ai across (physics-primitives a)
        minimize (distance ai b)))

(define-distance (rigid-shape rigid-shape)
  (loop for ai across (physics-primitives a)
        minimize (loop for bi across (physics-primitives b)
                       minimize (distance ai bi))))

(defmethod invalidate-global-bounds-cache :after ((entity rigid-shape))
  (loop for primitive across (physics-primitives entity)
        do (invalidate-global-bounds-cache primitive)))

(defmethod compute-bounding-box ((entity rigid-shape))
  (compute-bounding-box (physics-primitives entity)))

(defmethod compute-bounding-sphere ((entity rigid-shape))
  (compute-bounding-sphere (physics-primitives entity)))

(defmethod (setf physics-primitives) ((primitive primitive) (entity rigid-shape))
  (setf (physics-primitives entity) (vector primitive)))

(defmethod (setf physics-primitives) :before ((primitives vector) (entity rigid-shape))
  (loop for primitive across primitives
        do (setf (primitive-entity primitive) entity)))

(defmethod (setf physics-primitives) :after ((primitives vector) (entity rigid-shape))
  (%update-rigidbody-cache entity)
  (multiple-value-bind (center radius) (compute-bounding-sphere primitives)
    (setf (global-bounds-cache-sphere-offset (global-bounds-cache entity)) center)
    (setf (global-bounds-cache-radius (global-bounds-cache entity)) radius))
  (multiple-value-bind (center bsize) (compute-bounding-box primitives)
    (setf (global-bounds-cache-box-offset (global-bounds-cache entity)) center)
    (setf (global-bounds-cache-obb (global-bounds-cache entity)) bsize)))

(defmethod (setf physics-primitives) :around ((primitives vector) (entity rigid-shape))
  (let ((primitives (if (find 'general-mesh primitives :key #'type-of)
                        (convexify primitives)
                        primitives)))
    (call-next-method primitives entity)))

(defmethod (setf physics-primitives) ((mesh mesh-data) (entity rigid-shape))
  (setf (physics-primitives entity) (make-general-mesh :vertices (reordered-vertex-data mesh '(location))
                                                       :faces (simplify (faces mesh) '(unsigned-byte 16)))))

(defmethod (setf physics-primitives) ((primitives list) (entity rigid-shape))
  (setf (physics-primitives entity) (coerce primitives 'vector)))

(defmethod enter :after ((entity rigid-shape) (container container))
  (%update-rigidbody-cache entity))

(defmethod sample-volume ((entity rigid-shape) &optional vec)
  (let ((primitives (physics-primitives entity)))
    (case (length primitives)
      (0 (sampling:map-samples vec (lambda (vec) (global-location entity vec))))
      (1 (sample-volume (aref primitives 0) vec))
      (T ;; With multiple volumes we have to fall back to rejection sampling.
       (multiple-value-bind (loc bsize) (global-bounding-box)
         (flet ((generate (vec)
                  (!vrand vec loc bsize))
                (test (vec)
                  (loop for primitive across primitives
                        thereis (contains-p vec primitive))))
           (declare (dynamic-extent #'generate #'test))
           (sampling:rejection-sample #'generate #'test vec)))))))

(defun %update-primitive-transform (primitive entity)
  (declare (type primitive primitive) (type entity entity))
  (declare (optimize speed (safety 0)))
  (let ((joint (primitive-joint-index primitive))
        (base-transform (transform-matrix entity)))
    (declare (type mat4 base-transform))
    (declare (type (signed-byte 16) joint))
    (if (< joint 0)
        (!m* (primitive-global-transform primitive)
             base-transform
             (primitive-local-transform primitive))
        (let ((palette (palette entity)))
          (declare (type (simple-array mat4 (*)) palette))
          (when (< 0 (length palette))
            (!m* (primitive-global-transform primitive)
                 base-transform
                 (primitive-local-transform primitive)
                 (aref palette joint))
            (setf (awake-p entity) T))))
    (invalidate-global-bounds-cache primitive)))

(defmethod %update-rigidbody-cache ((entity rigid-shape))
  ;; TODO: ensure parent chain is updated first
  ;; TODO: how do we ensure this is called for shapes that are static (awake-p = NIL)
  ;;       but for which the parent changed?
  (let ((*model-matrix* (transform-matrix entity)))
    (!meye *model-matrix*)
    (apply-transforms entity))
  (setf (global-bounds-cache-dirty-p (global-bounds-cache entity)) T)
  (loop for primitive across (physics-primitives entity)
        do (%update-primitive-transform primitive entity)))

(defmethod detect-hits (other (entity rigid-shape) hits start end)
  (detect-hits other (physics-primitives entity) hits start end))

(defmethod detect-hits ((entity rigid-shape) other hits start end)
  (detect-hits (physics-primitives entity) other hits start end))

(defmethod global-transform-matrix ((entity rigid-shape) &optional target)
  (if target
      (m<- target (transform-matrix entity))
      (transform-matrix entity)))

(defmethod project-onto-surface ((primitive primitive) target &key (collision-mask 1) (direction -vy3+) (transform (transform)) (ignore primitive))
  (let ((opposite (v- direction))
        (location (vec3))
        (hit (make-hit)))
    (declare (dynamic-extent location opposite hit))
    (global-location primitive location)
    (global-support-function primitive opposite opposite)
    ;; We raycast from the opposite extreme end of the primitive to get the maximum tolerance for snapping.
    (when (raycast opposite direction :ignore ignore :target target :collision-mask collision-mask :hit hit)
      ;; The hit location is in global coordinates, so we need to relativise to the position of the object before moving
      ;; Get the support function to get our bottom, then extract the difference.
      (global-support-function primitive direction opposite)
      (!v+ (location transform) (!v- opposite location opposite) (hit-location hit))
      (when (v= (hit-normal hit) 0)
        (vsetf (hit-normal hit) 0 1 0))
      (nqalign (orientation transform) +vy3+ (hit-normal hit))
      (values transform (hit-depth hit)))))

(defmethod project-onto-surface ((object rigid-shape) target &key (collision-mask 1) (direction -vy3+) (transform (transform)))
  (%update-rigidbody-cache object)
  (let ((tf (transform))
        (min-depth most-positive-single-float))
    (declare (dynamic-extent tf))
    (loop for primitive across (the simple-vector (physics-primitives object))
          do (when (collision-mask-p collision-mask primitive)
               (multiple-value-bind (tf depth) (project-onto-surface primitive target :collision-mask collision-mask :direction direction :transform tf :ignore object)
                 (when (and tf (< depth min-depth))
                   (setf min-depth depth)
                   (let ((local (tfrom-mat (primitive-local-transform primitive))))
                     (!v- (tlocation transform) (tlocation tf) (tlocation local))
                     (!q* (trotation transform) (trotation tf) (nqinv (trotation local))))))))
    (when (< min-depth most-positive-single-float)
      (values transform min-depth))))

(defmethod snap-object-to-level ((object rigid-shape) &key (collision-mask 1) (direction -vy3+) (target (scene +main+)) (align-direction T))
  (let ((transform (tcopy (local-transform object))))
    (declare (dynamic-extent transform))
    (when (project-onto-surface object target :collision-mask collision-mask :direction direction :transform transform)
      (when align-direction
        (q<- (orientation object) (orientation transform)))
      (v<- (location object) (location transform))
      object)))

(defmethod start-frame ((entity rigid-shape))
  (%update-rigidbody-cache entity))

(defmethod integrate ((entity rigid-shape) dt)
  (%update-rigidbody-cache entity)
  (vsetf (force entity) 0 0 0))

(defclass rigidbody (rigid-shape)
  ((rotation :initform (vec 0 0 0) :reader rotation)
   (inverse-inertia-tensor :initform (mat3) :reader inverse-inertia-tensor)
   (torque :initform (vec 0 0 0) :reader torque)
   (angular-damping :initform 0.8 :accessor angular-damping)
   ;; Cache
   (world-inverse-inertia-tensor :initform (mat3) :reader world-inverse-inertia-tensor)
   (last-frame-acceleration :initform (vec 0 0 0) :reader last-frame-acceleration)))

(defmethod shared-initialize :after ((body rigidbody) slots &key inertia-tensor)
  (when inertia-tensor (setf (inertia-tensor body) inertia-tensor)))

(define-transfer rigidbody rotation inverse-inertia-tensor torque angular-damping)

(defmethod energy ((entity physics-entity))
  (* 0.5 (+ (* (mass entity) (vsqrlength (velocity entity)))
            (vsqrlength (m* (minv (inverse-inertia-tensor entity)) (rotation entity))))))

(defmethod (setf torque) ((torque vec3) (entity rigidbody))
  (v<- (torque entity) torque))

(defmethod (setf awake-p) :after ((false null) (entity rigidbody))
  (vsetf (rotation entity) 0 0 0))

(defmethod current-motion ((entity rigidbody))
  (+ (v. (velocity entity) (velocity entity))
     (v. (rotation entity) (rotation entity))))

(defmethod (setf rotation) ((vel vec3) (entity rigidbody))
  (v<- (rotation entity) vel))

(defmethod (setf inverse-inertia-tensor) ((mat mat3) (entity rigidbody))
  (m<- (inverse-inertia-tensor entity) mat))

(defmethod inertia-tensor ((entity rigidbody))
  (minv (inverse-inertia-tensor entity)))

(defmethod (setf inertia-tensor) ((mat mat3) (entity rigidbody))
  (let ((inv (minv mat)))
    (replace (marr3 (inverse-inertia-tensor entity)) (marr3 inv))))

(defmethod (setf inertia-tensor) ((primitive sphere) (entity rigidbody))
  (setf (inertia-tensor entity) (sphere-tensor (mass entity) (sphere-radius primitive))))

(defmethod (setf inertia-tensor) ((primitive ellipsoid) (entity rigidbody))
  (setf (inertia-tensor entity) (ellipsoid-tensor (mass entity) (ellipsoid-radius primitive))))

(defmethod (setf inertia-tensor) ((primitive box) (entity rigidbody))
  (setf (inertia-tensor entity) (box-tensor (mass entity) (box-bsize primitive))))

(defmethod (setf inertia-tensor) ((primitive cylinder) (entity rigidbody))
  (setf (inertia-tensor entity) (cylinder-tensor (mass entity) (cylinder-radius-bottom primitive) (cylinder-radius-top primitive) (cylinder-height primitive))))

(defmethod (setf inertia-tensor) ((primitive cone) (entity rigidbody))
  (setf (inertia-tensor entity) (cone-tensor (mass entity) (cone-radius primitive) (cone-height primitive))))

(defmethod (setf inertia-tensor) ((primitive pill) (entity rigidbody))
  (setf (inertia-tensor entity) (pill-tensor (mass entity) (pill-radius-bottom primitive) (pill-radius-top primitive) (pill-height primitive))))

(defmethod (setf inertia-tensor) ((primitive triangle) (entity rigidbody))
  (implement!))

(defmethod (setf inertia-tensor) ((primitive general-mesh) (entity rigidbody))
  (setf (inertia-tensor entity) (mesh-tensor (mass entity) (general-mesh-vertices primitive) (general-mesh-faces primitive))))

(defmethod (setf physics-primitives) :after ((primitive primitive) (entity rigidbody))
  (when (and (/= 0 (inverse-mass entity))
             (every #'zerop (marr3 (inverse-inertia-tensor entity))))
    (setf (inertia-tensor entity) primitive)))

(defmethod %update-rigidbody-cache :after ((rigidbody rigidbody))
  ;; NOTE: Re-normalising the orientation here aids in stability by eliminating drift.
  ;;       But doing so can lead to bi-stable states, as it'll change normalisation
  ;;       from frame to frame without the orientation being changed from outside.
  (nqunit* (orientation rigidbody))
  (compute-world-inertia-tensor (world-inverse-inertia-tensor rigidbody) (inverse-inertia-tensor rigidbody) (transform-matrix rigidbody)))

(defmethod impact-local ((entity rigidbody) force point)
  ;; NOTE: The FORCE direction is in world coordinates, and the POINT is in local coordinates
  (impact entity force (t*v (local-transform entity) point)))

(defmethod impact ((entity rigidbody) force point)
  (let ((local (v- point (location entity))))
    (nv+ (force entity) force)
    (nv+ (torque entity) (vc local force))))

(defmethod apply-force ((force spring-force) (entity rigidbody) dt)
  (let* ((lws (t*v (local-transform entity) (local-offset force)))
         (ows (t*v (local-transform (anchor force)) (anchor-offset force)))
         (force (v- lws ows))
         (coeff (* (spring-constant force) (abs (- (vlength force) (rest-length force))))))
    (impact entity (nv* (nvunit force) (- coeff)) lws)))

(defmethod apply-force ((force bungee-force) (entity rigidbody) dt)
  (let* ((lws (t*v (local-transform entity) (local-offset force)))
         (ows (t*v (local-transform (anchor force)) (anchor-offset force)))
         (force (v- lws ows))
         (coeff (* (spring-constant force) (- (vlength force) (rest-length force)))))
    (when (<= 0.0 coeff)
      (impact entity (nv* (nvunit force) (- coeff)) lws))))

(defmethod integrate ((entity rigidbody) dt)
  (let ((last-frame-acceleration (last-frame-acceleration entity))
        (angular-acceleration (m* (world-inverse-inertia-tensor entity) (torque entity))))
    (v<- last-frame-acceleration (force entity))
    (nv* (last-frame-acceleration entity) (inverse-mass entity))
    (nv+* (velocity entity) last-frame-acceleration dt)
    (nv+* (rotation entity) angular-acceleration dt)
    (nv* (velocity entity) (expt (damping entity) dt))
    (nv* (rotation entity) (expt (angular-damping entity) dt))
    (nv+* (location entity) (velocity entity) dt)
    (nv+* (location entity) last-frame-acceleration (* 0.5 dt dt))
    (nq+* (orientation entity) (rotation entity) dt)
    (nq+* (orientation entity) angular-acceleration (* 0.5 dt dt))
    (%update-rigidbody-cache entity)
    (vsetf (torque entity) 0 0 0)
    (vsetf (force entity) 0 0 0)))
