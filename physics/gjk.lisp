(in-package #:org.shirakumo.fraf.trial)

(defstruct (convex-mesh (:include primitive))
  (vertex-array (make-array '(0) :element-type single-float) :type (array single-float))
  (index-array (make-array '(0) :element-type integer) :type (array integer)))

(defgeneric support-mapping (primitive direction)
   (:documentation "Maps the direction into a supporting point for the object in that direction."))

(defmethod support-mapping ((sphere sphere) (direction vec3))
   (v+ (location sphere) (vunit direction)))

(defmethod support-mapping ((mesh convex-mesh) (direction vec3))
  (error "implement me"))

(defun support-mapping-minkowski-difference (object-1 object-2 direction)
  "Computes the support mapping of the Minkowski difference of object-1 with object-2."
  (v- (support-mapping object-1 direction)
      (support-mapping object-2 (v- direction))))

(defun gjk (a b)
  )
