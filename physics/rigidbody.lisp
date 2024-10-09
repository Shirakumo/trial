(in-package #:org.shirakumo.fraf.trial)

(defclass rigid-shape (physics-entity transformed-entity global-bounds-cached-entity)
  ((physics-primitives :initform #() :accessor physics-primitives)
   ;; Cache
   (transform-matrix :initform (mat4) :reader transform-matrix)
   (collision-mask :initform (1- (ash 1 32)) :accessor collision-mask)))

(defmethod shared-initialize :after ((body rigid-shape) slots &key physics-primitives)
  (when physics-primitives (setf (physics-primitives body) physics-primitives)))

(define-transfer rigid-shape
  (physics-primitives physics-primitives (lambda (p) (map-into (make-array (length p)) #'clone p))))

(define-hit-detector (rigid-shape primitive)
  (loop for ai across (physics-primitives a)
        do (detect-hits ai b)))

(define-hit-detector (rigid-shape ray)
  (loop for ai across (physics-primitives a)
        do (detect-hits b ai)))

(define-hit-detector (rigid-shape rigid-shape)
  (loop for ai across (physics-primitives a)
        do (loop for bi across (physics-primitives b)
                 do (detect-hits ai b))))

(define-distance (rigid-shape primitive)
  (loop for ai across (physics-primitives a)
        minimize (distance ai b)))

(define-distance (rigid-shape rigid-shape)
  (loop for ai across (physics-primitives a)
        minimize (loop for bi across (physics-primitives b)
                       minimize (distance ai bi))))

(defmethod invalidate-global-bounds-cache :after ((entity rigid-shape))
  (loop for primitive across (physics-primitives entity)
        do (invalidate-global-bounds-cache primitive)))

(defmethod bsize ((entity rigid-shape))
  (let ((vmin (vec3)) (vmax (vec3))
        (bsize (vec3)))
    (declare (dynamic-extent vmin vmax))
    (when (< 0 (length (physics-primitives entity)))
      (loop for primitive across (physics-primitives entity)
            do (let ((bsize (global-bsize primitive bsize))
                     (location (location primitive)))
                 (nvmin vmin (v- location bsize))
                 (nvmax vmax (v+ location bsize))))
      (cond ((or (<= most-positive-single-float (vx vmax))
                 (<= most-positive-single-float (vy vmax))
                 (<= most-positive-single-float (vz vmax)))
             (v<- bsize most-positive-single-float))
            (T
             (nv* (!v- bsize vmax vmin) 0.5)
             (assert (v/= bsize 0)))))
    bsize))

(defmethod bradius ((entity rigid-shape))
  (float (loop for primitive across (physics-primitives entity)
               for radius = (+ (vlength (location primitive)) (3ds:radius primitive))
               maximize radius)
         0f0))

(defmethod (setf physics-primitives) ((primitive primitive) (entity rigid-shape))
  (setf (physics-primitives entity) (vector primitive)))

(defmethod (setf physics-primitives) :before ((primitives vector) (entity rigid-shape))
  (loop for primitive across primitives
        do (setf (primitive-entity primitive) entity)))

(defmethod (setf physics-primitives) :after ((primitives vector) (entity rigid-shape))
  (%update-rigidbody-cache entity)
  (setf (global-bounds-cache-radius (global-bounds-cache entity)) (bradius entity))
  (setf (global-bounds-cache-obb (global-bounds-cache entity)) (bsize entity)))

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

(defmethod sample-volume ((entity rigid-shape) &optional vec)
  (let ((primitives (physics-primitives entity)))
    (case (length primitives)
      (0 (sampling:map-samples vec (lambda (vec) (global-location entity vec))))
      (1 (sample-volume (aref primitives 0) vec))
      (T ;; With multiple volumes we have to fall back to rejection sampling.
       (let ((loc (global-location entity))
             (bsize (global-bsize entity)))
         (flet ((generate (vec)
                  (!vrand vec loc bsize))
                (test (vec)
                  (loop for primitive across primitives
                        thereis (contains-p vec primitive))))
           (declare (dynamic-extent #'generate #'test))
           (sampling:rejection-sample #'generate #'test vec)))))))

(defmethod %update-rigidbody-cache ((entity rigid-shape))
  (setf (global-bounds-cache-dirty-p (global-bounds-cache entity)) T)
  (let ((*model-matrix* (transform-matrix entity)))
    (!meye *model-matrix*)
    (apply-transforms entity))
  (loop for primitive across (physics-primitives entity)
        do (!m* (primitive-transform primitive)
                (transform-matrix entity)
                (primitive-local-transform primitive))
           (invalidate-global-bounds-cache primitive)))

(defmethod detect-hits (other (entity rigid-shape) hits start end)
  (detect-hits other (physics-primitives entity) hits start end))

(defmethod detect-hits ((entity rigid-shape) other hits start end)
  (detect-hits (physics-primitives entity) other hits start end))

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
  (setf (inertia-tensor entity) (cylinder-tensor (mass entity) (cylinder-radius primitive) (cylinder-height primitive))))

(defmethod (setf inertia-tensor) ((primitive cone) (entity rigidbody))
  (setf (inertia-tensor entity) (cone-tensor (mass entity) (cone-radius primitive) (cone-height primitive))))

(defmethod (setf inertia-tensor) ((primitive pill) (entity rigidbody))
  (setf (inertia-tensor entity) (pill-tensor (mass entity) (pill-radius primitive) (pill-height primitive))))

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
  (impact entity force (t*v (tf entity) point)))

(defmethod impact ((entity rigidbody) force point)
  (let ((local (v- point (location entity))))
    (nv+ (force entity) force)
    (nv+ (torque entity) (vc local force))))

(defmethod apply-force ((force spring-force) (entity rigidbody) dt)
  (let* ((lws (t*v (tf entity) (local-offset force)))
         (ows (t*v (tf (anchor force)) (anchor-offset force)))
         (force (v- lws ows))
         (coeff (* (spring-constant force) (abs (- (vlength force) (rest-length force))))))
    (impact entity (nv* (nvunit force) (- coeff)) lws)))

(defmethod apply-force ((force bungee-force) (entity rigidbody) dt)
  (let* ((lws (t*v (tf entity) (local-offset force)))
         (ows (t*v (tf (anchor force)) (anchor-offset force)))
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

(defmethod start-frame ((entity rigidbody))
  (%update-rigidbody-cache entity))
