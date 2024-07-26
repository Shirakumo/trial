(in-package #:org.shirakumo.fraf.trial)

(defgeneric global-transform-matrix (entity &optional target))
(defgeneric global-location (entity &optional target))
(defgeneric global-orientation (entity &optional target))
(defgeneric global-bsize (entity &optional target))
(defgeneric global-bbox (entity))
(defgeneric global-radius (entity))
(defgeneric invalidate-global-bounds-cache (object))

(defmethod global-transform-matrix ((entity entity) &optional (matrix (meye 4)))
  (with-pushed-matrix ((model-matrix matrix))
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

(defmethod global-bbox (object)
  (let ((location (global-location object))
        (bsize (global-bsize object)))
    (cons (v- location bsize)
          (v+ location bsize))))

(defmethod global-radius (object)
  (let ((vec (vec3)))
    (declare (dynamic-extent vec))
    (vlength (global-bsize object vec))))

;; Default empty method to allow for a simple traversal of containers
(defmethod invalidate-global-bounds-cache (object))

(defmethod invalidate-global-bounds-cache :after ((container container))
  (sequences:dosequence (child container)
    (invalidate-global-bounds-cache child)))

(defstruct (global-bounds-cache
            (:constructor %make-global-bounds-cache (&optional generator (radius 0f0) (obb (vec3)))))
  (generator NIL :type T)
  (radius 0f0 :type single-float)
  (obb (vec3) :type vec3)
  (location (vec3) :type vec3)
  (aabb (vec3) :type vec3)
  (dirty-p T :type boolean))

(define-transfer global-bounds-cache
  global-bounds-cache-generator
  global-bounds-cache-radius
  (:eval (v<- (global-bounds-cache-obb target) (global-bounds-cache-obb source))
         (v<- (global-bounds-cache-aabb target) (global-bounds-cache-aabb source))
         (v<- (global-bounds-cache-location target) (global-bounds-cache-location source))))

(defun make-global-bounds-cache (generator &key (radius (bradius generator)) (bsize (bsize generator)))
  (%make-global-bounds-cache generator radius (vcopy bsize)))

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

(defmethod global-radius ((cache global-bounds-cache))
  (global-bounds-cache-radius cache))

(defmethod bsize ((cache global-bounds-cache))
  (global-bounds-cache-aabb cache))

(defmethod bradius ((cache global-bounds-cache))
  (global-bounds-cache-radius cache))

(defmethod invalidate-global-bounds-cache ((cache global-bounds-cache))
  (setf (global-bounds-cache-dirty-p cache) T))

(defmethod 3ds:location ((cache global-bounds-cache))
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (global-bounds-cache-location cache))

(defmethod 3ds:bsize ((cache global-bounds-cache))
  (when (global-bounds-cache-dirty-p cache)
    (update-global-bounds-cache cache))
  (global-bounds-cache-aabb cache))

(defmethod 3ds:radius ((cache global-bounds-cache))
  (global-bounds-cache-radius cache))

(defclass global-bounds-cached-entity (entity)
  ((global-bounds-cache :accessor global-bounds-cache)))

(defmethod shared-initialize :after ((entity global-bounds-cached-entity) slots &key)
  (unless (slot-boundp entity 'global-bounds-cache)
    (setf (global-bounds-cache entity) (make-global-bounds-cache entity))))

(defmethod global-location ((entity global-bounds-cached-entity) &optional target)
  (global-location (global-bounds-cache entity) target))

(defmethod global-bsize ((entity global-bounds-cached-entity) &optional target)
  (global-bsize (global-bounds-cache entity) target))

(defmethod global-radius ((entity global-bounds-cached-entity))
  (global-bounds-cache-radius (global-bounds-cache entity)))

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
