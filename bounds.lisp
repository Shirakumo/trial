(in-package #:org.shirakumo.fraf.trial)

(defgeneric location (entity))
(defgeneric orientation (entity))
(defgeneric bsize (entity))
(defgeneric bradius (entity))

(defgeneric compute-bounding-box (entity))
(defgeneric compute-bounding-sphere (entity))

(defgeneric global-transform-matrix (entity &optional target))
(defgeneric global-location (entity &optional target))
(defgeneric global-orientation (entity &optional target))
(defgeneric global-bsize (entity &optional target))
(defgeneric global-bradius (entity))
(defgeneric global-bounding-box (entity &optional center bsize))
(defgeneric global-bounding-sphere (entity &optional center))
(defgeneric invalidate-global-bounds-cache (object))

;;; Defaults
(defmethod bradius ((entity entity))
  (vlength (bsize entity)))

(defmethod compute-bounding-box ((entity entity))
  (values (vec3 0) (bsize entity)))

(defmethod compute-bounding-sphere ((entity entity))
  (values (vec3 0) (bradius entity)))

(defmethod compute-bounding-box ((sequence sequences:sequence))
  ;; Note: have to offset by location since the bbox is local to the child transform.
  (case (length sequence)
    (0
     (values (vec3) (vec3)))
    (1
     (let ((entity (elt sequence 0)))
       (multiple-value-bind (center bsize) (compute-bounding-box entity)
         (values (nv+ center (location entity)) bsize))))
    (T
     (let ((min (vec3 most-positive-single-float))
           (max (vec3 most-negative-single-float))
           (tmp (vec3))
           (center (vec3))
           (bsize (vec3)))
       (declare (dynamic-extent min max tmp))
       (sequences:dosequence (child sequence)
         (multiple-value-bind (center bsize) (compute-bounding-box child)
           (nvmin min (!v- tmp (!v+ tmp center (location child)) bsize))
           (nvmax max (!v+ tmp (!v+ tmp center (location child)) bsize))))
       (nv* (!v- bsize max min) 0.5)
       (!v+ center min bsize)
       (values center bsize)))))

(defmethod compute-bounding-sphere ((sequence sequences:sequence))
  ;; Note: have to offset by location since the bbox is local to the child transform.
  (case (length sequence)
    (0
     (values (vec3) 0.0))
    (1
     (let ((entity (elt sequence 0)))
       (multiple-value-bind (center radius) (compute-bounding-sphere entity)
         (values (nv+ center (location entity)) radius))))
    (T
     (let ((scalar (/ (length sequence)))
           (center (vec3))
           (radius 0.0) (tmp (vec3)))
       (declare (dynamic-extent tmp))
       ;; Note: this is not an optimal bounding sphere but rather a trivial approximation
       ;;       by center average
       (v<- center 0)
       (sequences:dosequence (child sequence)
         (nv+* center (global-bounding-sphere child tmp) scalar))
       (sequences:dosequence (child sequence)
         (multiple-value-bind (child-center child-radius) (compute-bounding-sphere child)
           (!v+ tmp child-center (location child))
           (setf radius (max radius (+ child-radius (vdistance tmp center))))))
       (values center radius)))))

(defmethod global-transform-matrix ((entity entity) &optional (matrix (meye 4)))
  (with-pushed-matrix ((model-matrix matrix))
    (!meye matrix)
    (apply-transforms entity)
    matrix))

(defmethod global-location ((entity entity) &optional (target (vec3)))
  (let ((matrix (meye 4)))
    (declare (dynamic-extent matrix))
    (global-transform-matrix entity matrix)
    (mcol3 matrix 3 target)
    target))

(defmethod global-orientation ((entity entity) &optional (target (quat)))
  (let ((matrix (meye 4)))
    (declare (dynamic-extent matrix))
    (global-transform-matrix entity matrix)
    (!qfrom-mat target matrix)))

(defmethod global-bsize ((entity entity) &optional (target (vec3)))
  (let ((mat (mat4)))
    (declare (dynamic-extent mat))
    (global-transform-matrix entity mat)
    (!m*4/3 target matrix (bsize object))))

(defmethod global-bradius ((entity entity))
  (let ((mat (mat4)))
    (declare (dynamic-extent mat))
    (global-transform-matrix entity mat)
    (with-fast-matref (m mat)
      (* (bradius entity) (max (m 0 0) (m 1 1) (m 2 2))))))

(defmethod global-bounding-box ((entity entity) &optional center bsize)
  (values (global-location entity center)
          (global-bsize entity bsize)))

(defmethod global-bounding-sphere ((entity entity) &optional center)
  (values (global-location entity center)
          (global-bradius entity)))

(defmethod global-bounding-box ((sequence sequences:sequence) &optional (center (vec3)) (bsize (vec3)))
  (case (length sequence)
    (0 (call-next-method))
    (1 (global-bounding-box (elt sequence 0) center bsize))
    (T
     (let ((min (vec3 most-positive-single-float))
           (max (vec3 most-negative-single-float))
           (tmp (vec3)))
       (declare (dynamic-extent min max tmp))
       (sequences:dosequence (child sequence)
         (global-bounding-box child center bsize)
         (!v- tmp center bsize) (nvmin min tmp)
         (!v+ tmp center bsize) (nvmax max tmp))
       (nv* (!v- bsize max min) 0.5)
       (!v+ center min bsize)
       (values center bsize)))))

(defmethod global-bounding-sphere ((sequence sequences:sequence) &optional (center (vec3)))
  (case (length sequence)
    (0 (call-next-method))
    (1 (global-bounding-sphere (elt sequence 0) center))
    (T
     (let ((scalar (/ (length sequence)))
           (radius 0.0) (tmp (vec3)))
       (declare (dynamic-extent tmp))
       ;; Note: this is not an optimal bounding sphere but rather a trivial approximation
       ;;       by center average
       (v<- center 0)
       (sequences:dosequence (child sequence)
         (nv+* center (global-bounding-sphere child tmp) scalar))
       (sequences:dosequence (child sequence)
         (multiple-value-bind (child child-radius) (global-bounding-sphere child tmp)
           (setf radius (max radius (+ child-radius (vdistance child center))))))
       (values center radius)))))

;; Default empty method to allow for a simple traversal of containers
(defmethod invalidate-global-bounds-cache (object))

(defmethod invalidate-global-bounds-cache :after ((container container))
  (sequences:dosequence (child container)
    (invalidate-global-bounds-cache child)))

(defstruct (global-bounds-cache
            (:constructor %make-global-bounds-cache (&optional generator (radius 0f0) (obb (vec3)) (sphere-offset (vec3)) (box-offset (vec3)))))
  (generator NIL :type T)
  (sphere-offset (vec3) :type vec3)
  (radius 0f0 :type single-float)
  (box-offset (vec3) :type vec3)
  (obb (vec3) :type vec3)
  (location (vec3) :type vec3)
  (aabb (vec3) :type vec3)
  (dirty-p T :type boolean))

(define-transfer global-bounds-cache
  global-bounds-cache-generator
  global-bounds-cache-radius
  (:eval (v<- (global-bounds-cache-obb target) (global-bounds-cache-obb source))
         (v<- (global-bounds-cache-aabb target) (global-bounds-cache-aabb source))
         (v<- (global-bounds-cache-location target) (global-bounds-cache-location source))
         (v<- (global-bounds-cache-sphere-offset target) (global-bounds-cache-sphere-offset source))
         (v<- (global-bounds-cache-box-offset target) (global-bounds-cache-box-offset source))))

(defun make-global-bounds-cache (generator &key radius bsize (sphere-offset (vec3) sphere-offset-p) (box-offset (vec3) box-offset-p))
  (unless radius
    (multiple-value-bind (center new-radius) (compute-bounding-sphere generator)
      (unless sphere-offset-p (v<- sphere-offset center))
      (setf radius new-radius)))
  (unless bsize
    (multiple-value-bind (center new-bsize) (compute-bounding-box generator)
      (unless box-offset-p (v<- box-offset center))
      (setf bsize new-bsize)))
  (%make-global-bounds-cache generator radius (vcopy bsize) (vcopy sphere-offset) (vcopy box-offset)))

(defun update-global-bounds-cache (cache)
  (let ((matrix (meye 4)))
    (declare (dynamic-extent matrix))
    (global-transform-matrix (global-bounds-cache-generator cache) matrix)
    (mcol3 matrix 3 (global-bounds-cache-location cache))
    (nmapply matrix #'abs)
    #+:check-global-bounds-cache-obb
    (assert (v/= 0.0 (global-bounds-cache-obb cache)))
    (!m*4/3 (global-bounds-cache-aabb cache) matrix (global-bounds-cache-obb cache)))
  (setf (global-bounds-cache-dirty-p cache) NIL))

(defmethod global-location ((cache global-bounds-cache) &optional target)
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (etypecase target
    (vec3 (v<- target (global-bounds-cache-location cache)))
    (null (global-bounds-cache-location cache))))

(defmethod global-bsize ((cache global-bounds-cache) &optional target)
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (etypecase target
    (vec3 (v<- target (global-bounds-cache-aabb cache)))
    (null (global-bounds-cache-aabb cache))))

(defmethod bsize ((cache global-bounds-cache))
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  ;; This is local to the cached origin, so we need to increase the bounds by the offset
  (nv+ (vabs (global-bounds-cache-box-offset cache))
       (global-bounds-cache-aabb cache)))

(defmethod bradius ((cache global-bounds-cache))
  ;; Note: don't need to check for dirty, as it's invariant to rotation
  ;; This is local to the cached origin, so we need to increase the bounds by the offset
  (+ (vlength (global-bounds-cache-sphere-offset cache))
     (global-bounds-cache-radius cache)))

(defmethod global-bounding-box ((cache global-bounds-cache) &optional (location (vec3)) bsize)
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (values (!v+ location (global-bounds-cache-location cache) (global-bounds-cache-box-offset cache))
          (etypecase bsize
            (vec3 (v<- bsize (global-bounds-cache-aabb cache)))
            (null (global-bounds-cache-aabb cache)))))

(defmethod global-bounding-sphere ((cache global-bounds-cache) &optional (location (vec3)))
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (values (!v+ location (global-bounds-cache-location cache) (global-bounds-cache-sphere-offset cache))
          (global-bounds-cache-radius cache)))

(defmethod invalidate-global-bounds-cache ((cache global-bounds-cache))
  (setf (global-bounds-cache-dirty-p cache) T))

(defmethod 3ds:location ((cache global-bounds-cache))
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (global-bounds-cache-location cache))

(defmethod 3ds:bsize ((cache global-bounds-cache))
  (bsize cache))

(defmethod 3ds:radius ((cache global-bounds-cache))
  (bradius cache))

(defmethod 3ds:bounding-box ((cache global-bounds-cache))
  (global-bounding-box cache))

(defmethod 3ds:bounding-sphere ((cache global-bounds-cache))
  (global-bounding-sphere cache))

(defclass global-bounds-cached-entity (entity)
  ((global-bounds-cache :accessor global-bounds-cache)))

(defmethod shared-initialize :after ((entity global-bounds-cached-entity) slots &key)
  (unless (slot-boundp entity 'global-bounds-cache)
    (setf (global-bounds-cache entity) (make-global-bounds-cache entity))))

(defmethod global-location ((entity global-bounds-cached-entity) &optional target)
  (global-location (global-bounds-cache entity) target))

(defmethod global-bsize ((entity global-bounds-cached-entity) &optional target)
  (global-bsize (global-bounds-cache entity) target))

(defmethod global-bounding-box ((entity global-bounds-cached-entity) &optional (location (vec3)) bsize)
  (global-bounding-box (global-bounds-cache entity) location bsize))

(defmethod global-bounding-sphere ((entity global-bounds-cached-entity) &optional (location (vec3)))
  (global-bounding-sphere (global-bounds-cache entity) location))

(defmethod (setf location) :after ((new-value t) (entity global-bounds-cached-entity))
  (setf (global-bounds-cache-dirty-p (global-bounds-cache entity)) T))

(defmethod (setf orientation) :after ((new-value t) (entity global-bounds-cached-entity))
  (setf (global-bounds-cache-dirty-p (global-bounds-cache entity)) T))

(defmethod invalidate-global-bounds-cache ((cache global-bounds-cached-entity))
  (setf (global-bounds-cache-dirty-p (global-bounds-cache cache)) T))

(defmethod 3ds:location ((entity global-bounds-cached-entity))
  (global-location (global-bounds-cache entity)))

(defmethod 3ds:bsize ((entity global-bounds-cached-entity))
  (global-bsize (global-bounds-cache entity)))

(defmethod 3ds:radius ((entity global-bounds-cached-entity))
  (global-bounds-cache-radius (global-bounds-cache entity)))

(defmethod 3ds:bounding-box ((entity global-bounds-cached-entity))
  (global-bounding-box (global-bounds-cache entity)))

(defmethod 3ds:bounding-sphere ((entity global-bounds-cached-entity))
  (global-bounding-sphere (global-bounds-cache entity)))
