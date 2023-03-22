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
  (:documentation "Returns a single point from the object. Required for the GJK algorithm."))

(defmethod sample-one-point ((sphere sphere)) (location sphere))

(defun point-of-minimum-norm-in-convex-hull (set)
  (error "implement me"))

(defun epsilon-= (a b &optional (epsilon 1e-6))
  (< (abs (v2norm (v- a b))) epsilon))

(defun epsilon-zerop (a &optional (epsilon 1e-6))
  (< (- epsilon) a epsilon))

(defun colinear-p (q1 q2 q3)
  (epsilon-zerop (v2norm (vc (v- q3 q1) (v- q2 q1)))))

(defun parallelepiped-zero-volume-p (q1 q2 q3 q4)
  (epsilon-zerop (v. (vc (v- q2 q1) (v- q3 q1)) (v- q4 q1))))

(defun reduce-to-smallest-spanning-set (p q)
  "Returns the smallest subset of q such that p lies in the complex hull of q."
  (ecase (length q)
    (4
     (let ((q1 (aref q 0))
           (q2 (aref q 1))
           (q3 (aref q 2))
           (q4 (aref q 3)))
       (cond
         ((parallelepiped-zero-volume-p q1 q2 q3 p)
          (reduce-to-smallest-spanning-set p (vector q1 q2 q3)))
         ((parallelepiped-zero-volume-p q1 q2 q4 p)
          (reduce-to-smallest-spanning-set p (vector q1 q2 q4)))
         ((parallelepiped-zero-volume-p q1 q3 q4 p)
          (reduce-to-smallest-spanning-set p (vector q1 q3 q4)))
         ((parallelepiped-zero-volume-p q2 q3 q4 p)
          (reduce-to-smallest-spanning-set p (vector q2 q3 q4)))
         (t (progn
              (warn "Returning a simplex of size 4")
              q)))))
    (3
     (let ((q1 (aref q 0))
           (q2 (aref q 1))
           (q3 (aref q 2)))
       (cond
         ((colinear-p q1 q2 p) (reduce-to-smallest-spanning-set p (vector q1 q2)))
         ((colinear-p q2 q3 p) (reduce-to-smallest-spanning-set p (vector q2 q3)))
         ((colinear-p q3 q1 p) (reduce-to-smallest-spanning-set p (vector q3 q1)))
         (t q))))
    (2
     (let ((q1 (aref q 0))
           (q2 (aref q 1)))
       (cond
         ((epsilon-= p q1) (vector q1))
         ((epsilon-= p q2) (vector q2))
         (t q))))
    (1 q)))

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
