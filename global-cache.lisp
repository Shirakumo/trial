(in-package #:org.shirakumo.fraf.trial)

(defgeneric global-location (entity &optional target))
(defgeneric global-orientation (entity &optional target))
(defgeneric global-bsize (entity &optional target))
(defgeneric global-bbox (entity))

(defmethod global-bbox (object)
  (let ((location (global-location object))
        (bsize (global-bsize object)))
    (cons (v- location bsize)
          (v+ location bsize))))

(defstruct (global-cache
            (:constructor (%make-global-cache (generator radius obb))))
  (generator NIL :type T)
  (radius 0f0 :type single-float)
  (obb (vec3) :type vec3)
  (location (vec3) :type vec3)
  (aabb (vec3) :type vec3)
  (dirty-p T :type boolean))

(defun make-global-cache (generator)
  (%make-global-cache generator (radius generator) (vcopy (bsize generator))))

(defun update-global-cache (cache)
  (global-location (global-cache-generator cache) (global-cache-location cache))
  (let ((orientation (quat)))
    (declare (dynamic-extent orientation))
    (global-orientation (global-cache-generator cache) orientation)
    (!q* (global-cache-aabb cache) orientation (global-cache-obb cache)))
  (setf (global-cache-dirty-p cache) NIL))

(defmethod global-location ((cache global-cache) &optional target)
  (when (global-cache-dirty-p cache)
    (update-global-cache cache))
  (etypecase target
    (vec3 (v<- target (global-cache-location cache)))
    (null (global-cache-location cache))))

(defmethod global-bsize ((cache global-cache) &optional target)
  (when (global-cache-dirty-p cache)
    (update-global-cache cache))
  (etypecase target
    (vec3 (v<- target (global-cache-aabb cache)))
    (null (global-cache-aabb cache))))

(defmethod radius ((cache global-cache))
  (global-cache-radius cache))

(defmethod 3ds:location ((cache global-cache))
  (global-cache-location cache))

(defmethod 3ds:bsize ((cache global-cache))
  (global-cache-aabb cache))

(defmethod 3ds:radius ((cache global-cache))
  (global-cache-radius cache))
