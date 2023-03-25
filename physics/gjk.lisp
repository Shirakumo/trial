(in-package #:org.shirakumo.fraf.trial)

(defstruct (convex-mesh (:include primitive))
  (vertex-array (make-array '(0) :element-type single-float) :type (array single-float))
  (index-array (make-array '(0) :element-type integer) :type (array integer)))

(defgeneric support-mapping (primitive direction)
   (:documentation "Maps the direction into a supporting point for the object in that direction."))

(defmethod support-mapping ((sphere sphere) (direction vec3))
   (v+ (location sphere) (vunit direction)))

(defmethod support-mapping ((mesh convex-mesh) (direction vec3))
  (with-slots (vertex-array index-array) mesh
    (let ((n (/ (length index-array) 3))
          (best-point nil)
          (best-score most-negative-double-float))
      (loop for i from 0 below n do
            (let ((point (vec3 (aref vertex-array (aref index-array (* 3 i)))
                               (aref vertex-array (aref index-array (+ 1 (* 3 i))))
                               (aref vertex-array (aref index-array (+ 2 (* 3 i)))))))
              (when (<= best-score (v. direction point))
                (setf best-point point)
                (setf best-score (v. direction point)))))
      best-point)))

(defun support-mapping-minkowski-difference (object-1 object-2 direction)
  "Computes the support mapping of the Minkowski difference of object-1 with object-2."
  (v- (support-mapping object-1 direction)
      (support-mapping object-2 (v- direction))))

(defgeneric sample-one-point (object)
  (:documentation "Returns a single point from the object. Required for the GJK algorithm."))

(defmethod sample-one-point ((sphere sphere)) (location sphere))
(defmethod sample-one-point ((mesh convex-mesh))
  (with-slots (vertex-array index-array) mesh
    (vec3 (aref vertex-array (aref index-array 0))
          (aref vertex-array (aref index-array 1))
          (aref vertex-array (aref index-array 2)))))

(defun closest-point-to-origin-on-line (q1 q2)
  (let ((n (vunit (v- q2 q1))))
    (v- q1 (v* n (v. q1 n)))))


(defun closest-point-to-origin-on-plane (p n)
  (v* (/ (v. p n) (v. n n)) n))

(defun point-of-minimum-norm-in-convex-hull (set)
  (labels ((voronoi-half-plane (q1 q2)
             (v. (v- q2 q1) (v- q1)))
           (normal-vector (q1 q2 q3)
             (vc (v- q2 q1) (v- q3 q1)))
           (voronoi-edge-helper (q1 q2 q3)
             (v. (v- q1)
                 (vc (v- q2 q1)
                     (normal-vector q1 q2 q3))))
           (voronoi-edge (q1 q2 q3 &optional q4)
             (if q4
                 (and
                  (<= 0 (voronoi-half-plane q1 q2))
                  (<= 0 (voronoi-half-plane q2 q1))
                  (<= 0 (voronoi-edge-helper q1 q2 q3))
                  (<= 0 (voronoi-edge-helper q1 q2 q4)))
                 (and
                  (<= 0 (voronoi-half-plane q1 q2))
                  (<= 0 (voronoi-half-plane q2 q1))
                  (<= 0 (voronoi-edge-helper q1 q2 q3)))))
           (voronoi-face (q1 q2 q3 q4)
             (< (* (v. (v- q1)
                       (normal-vector q1 q2 q3))
                   (v. (v- q4 q1)
                       (normal-vector q1 q2 q3)))
                0))
           (voronoi-point (q1 q2 q3 &optional q4)
             (if q4
                 (and (<= (voronoi-half-plane q1 q2) 0)
                      (<= (voronoi-half-plane q1 q3) 0)
                      (<= (voronoi-half-plane q1 q4) 0))
                 (and (<= (voronoi-half-plane q1 q2) 0)
                      (<= (voronoi-half-plane q1 q3) 0)))))
    (ecase (length set)
      (4
       (let* ((q1 (aref set 0))
              (q2 (aref set 1))
              (q3 (aref set 2))
              (q4 (aref set 3)))
         (cond
           ;; q1
           ((voronoi-point q1 q2 q3 q4) q1)
           ;; q2
           ((voronoi-point q2 q1 q3 q4) q2)
           ;; q3
           ((voronoi-point q3 q1 q2 q4) q3)
           ;; q4
           ((voronoi-point q4 q1 q2 q3) q4)
           ;; Edges
           ;; q1 q2
           ((voronoi-edge q1 q2 q3 q4) (closest-point-to-origin-on-line q1 q2))
           ;; q1 q3
           ((voronoi-edge q1 q3 q2 q4)  (closest-point-to-origin-on-line q1 q3))
           ;; q1 q4
           ((voronoi-edge q1 q4 q2 q3) (closest-point-to-origin-on-line q1 q4) )
           ;; q2 q3
           ((voronoi-edge q2 q3 q1 q4)  (closest-point-to-origin-on-line q2 q3))
           ;; q2 q4
           ((voronoi-edge q2 q4 q1 q3)  (closest-point-to-origin-on-line q2 q4))
           ;; q3 q4
           ((voronoi-edge q3 q4 q1 q2)  (closest-point-to-origin-on-line q3 q4))
;;; Faces
           ;; q1 q2 q3
           ((voronoi-face q1 q2 q3 q4) (closest-point-to-origin-on-plane q1 (normal-vector q1 q2 q3)))
           ;; q1 q2 q4
           ((voronoi-face q1 q2 q4 q3) (closest-point-to-origin-on-plane q1 (normal-vector q1 q2 q4)))
           ;; q1 q3 q4
           ((voronoi-face q1 q3 q4 q2) (closest-point-to-origin-on-plane q1 (normal-vector q1 q3 q4)))
           ;; q2 q3 q4
           ((voronoi-face q2 q3 q4 q1) (closest-point-to-origin-on-plane q2 (normal-vector q2 q3 q4)))
           ;; Simplex interior
           (t (vec3 0.0 0.0 0.0)))))
      (3 (let ((q1 (aref set 0))
               (q2 (aref set 1))
               (q3 (aref set 2)))
           (cond
             ;; q1
             ((voronoi-point q1 q2 q3) q1)
             ;; q2
             ((voronoi-point q2 q1 q3) q2)
             ;; q3
             ((voronoi-point q3 q2 q1) q3)
             ;; q1 q2
             ((voronoi-edge q1 q2 q3) (point-of-minimum-norm-in-convex-hull (vector q1 q2)))
             ;; q1 q3
             ((voronoi-edge q1 q3 q2) (point-of-minimum-norm-in-convex-hull (vector q1 q3)))
             ;; q2 q3
             ((voronoi-edge q2 q3 q1) (point-of-minimum-norm-in-convex-hull (vector q2 q3)))
             ;; Triangle interior
             (t (closest-point-to-origin-on-plane q1 (normal-vector q1 q2 q3)))
             )))
      (2 (let ((q1 (aref set 0))
               (q2 (aref set 1)))
           (let* ((n (v- q2 q1))
                  (s (/ (v. n (v- q1)) (v. n n))))
             (v+ q1 (v* n (alexandria:clamp s 0.0 1.0))))))
      (1 (aref set 0)))))

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
