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

(defgeneric sample-one-point (object)
  (:documentation
   "Returns a single point from the object. Required for the GJK algorithm."))

(defmethod sample-one-point ((sphere sphere)) (location sphere))

(defun point-of-minimum-norm-in-convex-hull (set)
  (error "implement me"))

(defun reduce-to-smallest-spanning-set (p q)
  "Returns the smallest subset of q such that p lies in the complex hull of q."
  (error "implement me"))

(defun gjk (a b)
  (let ((q (vector (v- (sample-one-point a) (sample-one-point b))))) ; step 1
    (loop do
      (let ((p (point-of-minimum-norm-in-convex-hull q))) ; step 2
        (when (< (v2norm (v- p (vec3 0.0 0.0 0.0))) 1e-6) ; step 3
          (return-from gjk t))
        (setf q (reduce-to-smallest-spanning-set p q)) ; step 4
        (let ((v (support-mapping-minkowski-difference a b (v- p)))) ; step 5
          (when (>= (v. v p) (v. p p)) ; step 6
              (return-from gjk (values nil (v2norm p))))
          (vector-push-extend v q)))))) ; step 7
