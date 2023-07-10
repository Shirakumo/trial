(defpackage #:org.shirakumo.fraf.trial.v-clip
  (:use #:cl #:3d-vectors #:3d-matrices)
  (:export))

(in-package #:org.shirakumo.fraf.trial.v-clip)

(defstruct state
  (a (vec 0 0 0) :type vec3)
  (b (vec 0 0 0) :type vec3)
  (distance 0.0 :type single-float)
  (lf NIL :type T)
  (rf NIL :type T))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type T :identity T)
    (format stream "~a" (state-distance state))))

(defstruct (plane (:constructor %make-plane (normal offset)))
  (normal (vec 0 0 0) :type vec3)
  (offset 0.0 :type single-float))

(defun plane-distance (plane vertex)
  (+ (v. (plane-normal plane) vertex) (plane-offset plane)))

(defun make-plane (normal offset)
  (etypecase offset
    (real (%make-plane normal (float offset 0f0)))
    (vec3 (%make-plane normal (- (v. offset normal))))
    (vertex (make-plane normal (vertex-location offset)))))

(defmethod print-object ((plane plane) stream)
  (print-unreadable-object (plane stream :type T)
    (format stream "~a ~a" (plane-normal plane) (plane-offset plane))))

(defstruct cone
  (plane NIL :type plane)
  (neighbor NIL))

(defstruct (vertex-cone 
            (:include cone)
            (:constructor make-vertex-cone (plane neighbor))))

(defmethod print-object ((cone vertex-cone) stream)
  (print-unreadable-object (cone stream :type T)
    (format stream "~a ~a" (plane-normal (cone-plane cone)) (plane-offset (cone-plane cone)))))

(defstruct (face-cone 
            (:include cone)
            (:constructor make-face-cone (plane neighbor)))
  (ccw NIL)
  (cw NIL)
  (index 0 :type (integer 0 2)))

(defmethod print-object ((cone face-cone) stream)
  (print-unreadable-object (cone stream :type T)
    (format stream "~a ~a" (plane-normal (cone-plane cone)) (plane-offset (cone-plane cone)))))

(defstruct (vertex (:constructor %make-vertex (location)))
  (location (vec 0 0 0) :type vec3)
  (cone (make-array 0 :adjustable T :fill-pointer T) :type vector))

(defmethod print-object ((vertex vertex) stream)
  (print-unreadable-object (vertex stream :type T)
    (format stream "~a" (vertex-location vertex))))

(defun make-vertex (x y z)
  (%make-vertex (vec x y z)))

(defstruct (face (:constructor %make-face (plane)))
  (plane NIL :type plane)
  (cone (make-array 0 :adjustable T :fill-pointer T) :type vector))

(defmethod print-object ((face face) stream)
  (print-unreadable-object (face stream :type T)
    (format stream "~a ~a" (plane-normal (face-plane face)) (plane-offset (face-plane face)))))

(defun make-face (v0 v1 v2 edges)
  (let* ((u (v- (vertex-location v1) (vertex-location v0)))
         (v (v- (vertex-location v2) (vertex-location v1)))
         (face (%make-face (make-plane (nvunit (vc u v)) v0))))
    (make-edge face v0 v1 edges)
    (make-edge face v1 v2 edges)
    (make-edge face v2 v0 edges)
    (let ((cone (face-cone face)))
      (loop for i from 0 below (length cone)
            for cur = (aref cone i)
            do (setf (face-cone-index cur) i)
               (setf (face-cone-cw cur) (aref cone (mod (1+ i) (length cone))))
               (setf (face-cone-ccw cur) (aref cone (mod (1- i) (length cone))))))
    face))

(defstruct (edge (:constructor %make-edge))
  (tail NIL :type vertex)
  (head NIL :type vertex)
  (left NIL :type face)
  (right NIL :type face)
  (length 0.0 :type single-float)
  (direction (vec 0 0 0) :type vec3)
  (tplane NIL :type plane)
  (hplane NIL :type plane)
  (lplane NIL :type plane)
  (rplane NIL :type plane))

(defmethod print-object ((edge edge) stream)
  (print-unreadable-object (edge stream :type T)
    (format stream "~a ~a" (vertex-location (edge-tail edge)) (vertex-location (edge-head edge)))))

(defun make-edge (f tail head edges)
  (flet ((add-cone (cone vec)
           (vector-push-extend cone vec)))
    (loop for cone across (vertex-cone head)
          for edge = (cone-neighbor cone)
          do (when (eq tail (edge-head edge))
               (setf (edge-right edge) f)
               (let ((v (nvunit (vc (edge-direction edge) (plane-normal (face-plane f))))))
                 (setf (edge-rplane edge) (make-plane v (vertex-location head)))
                 (add-cone (make-face-cone (edge-rplane edge) edge) (face-cone f)))
               (return-from make-edge)))
    (let* ((dir (v- (vertex-location head) (vertex-location tail)))
           (edge (%make-edge :tail tail :head head :left f :right f
                             :length (vlength dir) :direction (nvunit dir)
                             :tplane (make-plane (v- dir) (vertex-location tail))
                             :hplane (make-plane dir (vertex-location head))
                             :lplane (make-plane (nvunit (vc (plane-normal (face-plane f)) dir)) (vertex-location tail))
                             :rplane (make-plane (vec 0 0 0) 0.0))))
      (vector-push-extend edge edges)
      (add-cone (make-vertex-cone (edge-tplane edge) edge) (vertex-cone tail))
      (add-cone (make-vertex-cone (edge-hplane edge) edge) (vertex-cone head))
      (add-cone (make-face-cone (edge-lplane edge) edge) (face-cone f)))))

(defstruct (transform-cache
            (:conc-name tf-))
  (feature NIL :type T)
  (location (vec 0 0 0) :type vec3)
  (tail (vec 0 0 0) :type vec3)
  (head (vec 0 0 0) :type vec3)
  (segment (vec 0 0 0) :type vec3))

(defun cache-vertex (mat vertex cache)
  (unless (eq vertex (tf-feature cache))
    (n*m mat (v<- (tf-location cache) (vertex-location vertex)))
    (setf (tf-feature cache) vertex)))

(defun cache-edge* (cache)
  (v<- (tf-segment cache) (tf-head cache))
  (nv- (tf-segment cache) (tf-tail cache)))

(defun cache-edge (mat edge cache)
  (unless (eq edge (tf-feature cache))
    (n*m mat (v<- (tf-tail cache) (vertex-location (edge-tail edge))))
    (n*m mat (v<- (tf-head cache) (vertex-location (edge-head edge))))
    (cache-edge* cache)
    (setf (tf-feature cache) edge)))

(defstruct mesh
  (vertices NIL :type (simple-array vertex (*)))
  (faces NIL :type (simple-array face (*)))
  (edges NIL :type (simple-array edge (*))))

(defmethod print-object ((mesh mesh) stream)
  (print-unreadable-object (mesh stream :type T)
    (format stream "~a verts ~a faces ~a edges"
            (length (mesh-vertices mesh))
            (length (mesh-faces mesh))
            (length (mesh-edges mesh)))))

(defun make-triangle-mesh (locations indices)
  (let* ((vertices (make-array (truncate (length locations) 3)))
         (faces (make-array (truncate (length indices) 3)))
         (edges (make-array 0 :adjustable T :fill-pointer T)))
    (loop for i from 0 below (length locations) by 3
          for j from 0
          for vertex = (make-vertex (aref locations (+ i 0))
                                    (aref locations (+ i 1))
                                    (aref locations (+ i 2)))
          do (setf (aref vertices j) vertex))
    (loop for i from 0 below (length indices) by 3
          for j from 0
          for face = (make-face (aref vertices (aref indices (+ i 0)))
                                (aref vertices (aref indices (+ i 1)))
                                (aref vertices (aref indices (+ i 2)))
                                edges)
          do (setf (aref faces j) face))
    (make-mesh :vertices vertices :faces faces :edges (make-array (length edges) :initial-contents edges))))

(defmethod make-primitive-mesh ((primitive trial:primitive))
  (make-primitive-mesh (trial::coerce-object primitive 'trial::convex-mesh)))

(defmethod make-primitive-mesh ((primitive trial::convex-mesh))
  (make-triangle-mesh (trial::convex-mesh-vertices primitive)
                      (trial::convex-mesh-faces primitive)))

(defmacro define-tester (name args &body body)
  `(defun ,name (state ,@(cddr args))
     (let ((,(first args) (state-lf state))
           (,(second args) (state-rf state)))
       (flet ((next ()
                (setf (state-lf state) ,(first args))
                (setf (state-rf state) ,(second args))
                (return-from ,name :continue))
              (finish (result &optional a b distance)
                (when a (v<- (state-a state) a))
                (when b (v<- (state-b state) b))
                (setf (state-distance state) (or distance
                                                 (vdistance (state-a state) (state-b state))))
                (return-from ,name result)))
         (declare (inline next finish))
         (declare (ignorable #'next #'finish))
         ,@body))))

(define-tester vertex-vertex (v1 v2 cv1 cv2 m1 m2)
  (cache-vertex m2 v2 cv2)
  (loop for cone across (vertex-cone v1)
        do (when (< (plane-distance (cone-plane cone) (tf-location cv2)) 0)
             (setf v1 (cone-neighbor cone))
             (next)))
  
  (cache-vertex m1 v1 cv1)
  (loop for cone across (vertex-cone v2)
        do (when (< (plane-distance (cone-plane cone) (tf-location cv1)) 0)
             (setf v2 (cone-neighbor cone))
             (next)))

  (finish (if (< 0 (state-distance state)) :disjoint :penetration)
          (vertex-location v1)
          (vertex-location v2)))

(define-tester vertex-face (v f cv m mesh)
  (cache-vertex m v cv)
  (let ((updated NIL)
        (dmin 0.0))
    (loop for cone across (face-cone f)
          do (let ((d (plane-distance (cone-plane cone) (tf-location cv))))
               (when (< d dmin)
                 (setf f (cone-neighbor cone))
                 (setf dmin d)
                 (setf updated T))))
    (when updated (next)))

  (let ((d (plane-distance (face-plane f) (tf-location cv)))
        (d2 0.0)
        (xother (vec3 0 0 0)))
    (declare (dynamic-extent xother))
    (when (= 0 d)
      (finish :penetration
              (vertex-location v)
              (tf-location cv)))
    (loop for cone across (vertex-cone v)
          for edge = (cone-neighbor cone)
          do (v<- xother (vertex-location
                          (if (eq v (edge-tail edge))
                              (edge-head edge)
                              (edge-tail edge))))
             (n*m m xother)
             (setf d2 (plane-distance (face-plane f) xother))
             (when (or (and (< d 0) (< d d2))
                       (and (< 0 d) (< d2 d)))
               (v<- (tf-tail cv) (tf-location cv))
               (v<- (tf-head cv) xother)
               (unless (eq v (edge-tail edge))
                 (rotatef (tf-tail cv) (tf-head cv)))
               (cache-edge* cv)
               (setf (tf-feature cv) edge)
               (setf v edge)
               (next)))
    (when (< 0 d)
      (finish :disjoint
              (vertex-location v)
              (nv+* (vcopy (tf-location cv))
                    (plane-normal (face-plane f)) (- d))
              d))

    (loop for face across (mesh-faces mesh)
          for d2 = (plane-distance (face-plane face) (tf-location cv))
          do (when (< d d2)
               (setf d d2)
               (setf f face)))
    (when (< 0 d)
      (next))
    (finish :penetration
            (vertex-location v)
            (nv+* (tf-location cv) (plane-normal (face-plane f)) (- d))
            d)))

(define-tester vertex-edge (v e cv ce mve mev)
  (cache-vertex mve v cv)
  (when (< 0 (plane-distance (edge-tplane e) (tf-location cv)))
    (setf e (edge-tail e)) (next))
  (when (< 0 (plane-distance (edge-hplane e) (tf-location cv)))
    (setf e (edge-head e)) (next))
  (when (< 0 (plane-distance (edge-lplane e) (tf-location cv)))
    (setf e (edge-left e)) (next))
  (when (< 0 (plane-distance (edge-rplane e) (tf-location cv)))
    (setf e (edge-right e)) (next))

  (cache-edge mev e ce)
  (let ((min 0) (max 1) min-neighbor max-neighbor)
    (loop for cone across (vertex-cone v)
          for dt = (plane-distance (cone-plane cone) (tf-tail ce))
          for dh = (plane-distance (cone-plane cone) (tf-head ce))
          do (if (<= 0 dt)
                 (when (and (not (<= 0 dh)) (< (/ dt (- dt dh)) max))
                   (setf max (/ dt (- dt dh)))
                   (setf max-neighbor (cone-neighbor cone))
                   (when (< max min) (return)))
                 (cond ((< dh 0)
                        (setf min-neighbor (cone-neighbor cone))
                        (setf max-neighbor (cone-neighbor cone)))
                       ((< min (/ dt (- dt dh)))
                        (setf min (/ dt (- dt dh)))
                        (setf min-neighbor (cone-neighbor cone))
                        (when (< max min) (return))))))

    (when (and min-neighbor (eq min-neighbor max-neighbor))
      (setf v min-neighbor)
      (next))

    (when min-neighbor
      (let ((offset (nv- (nv+* (vcopy (edge-tail e)) (tf-segment ce) min)
                         (vertex-location v))))
        (when (v= offset 0)
          (finish :penetration
                  (vertex-location v)
                  (tf-location cv)))
        (when (< 0 (v. offset (tf-segment ce)))
          (setf v min-neighbor)
          (next))))

    (when max-neighbor
      (let ((offset (nv- (nv+* (vcopy (edge-tail e)) (tf-segment ce) max)
                         (vertex-location v))))
        (when (v= offset 0)
          (finish :penetration
                  (vertex-location v)
                  (tf-location cv)))
        (when (< 0 (v. offset (tf-segment ce)))
          (setf v max-neighbor)
          (next))))

    (let ((h (v- (tf-location cv)
                 (vertex-location (edge-tail e)))))
      (finish :disjoint
              (vertex-location v)
              (nv+* (vcopy (vertex-location (edge-tail e)))
                    (edge-direction e) (v. h (edge-direction e)))
              (vdistance (state-b state) (tf-location cv))))))

(defun edge-edge-subtest (e ce cp)
  (flet ((next (v)
           (return-from edge-edge-subtest
             (values v e cp))))
    (let ((min 0) (max 1) min-neighbor max-neighbor)
      ;; Check tail and head planes
      (flet ((clip (plane neighbor)
               (let ((dt (- (plane-distance plane (tf-tail ce))))
                     (dh (- (plane-distance plane (tf-head ce)))))
                 (cond ((< dt 0)
                        (when (< dh 0)
                          (setf e neighbor)
                          (next :continue))
                        (setf min (/ dt (- dt dh)))
                        (setf min-neighbor neighbor))
                       ((< dh 0)
                        (setf max (/ dt (- dt dh)))
                        (setf max-neighbor neighbor))))))
        (clip (edge-tplane e) (edge-tail e))
        (clip (edge-hplane e) (edge-head e)))
      (let ((vmin-neighbor min-neighbor)
            (vmax-neighbor max-neighbor)
            (vmin min) (vmax max))
        ;; Check left and right planes
        (labels ((vertex (extremum neighbor comp)
                   (let ((p (nv- (nv+ (v* (tf-segment ce) extremum)
                                      (tf-tail ce))
                                 (vertex-location neighbor))))
                     (when (v= p 0)
                       ;; FIXME: return changed values
                       (setf cp (vertex-location neighbor))
                       (next :penetration))
                     (when (funcall comp (v. p (tf-segment ce)))
                       (setf e neighbor)
                       (next :continue))))
                 (side (plane neighbor)
                   (let ((dt (- (plane-distance plane (tf-tail ce))))
                         (dh (- (plane-distance plane (tf-head ce)))))
                     (cond ((< dt 0)
                            (cond ((< dh 0)
                                   (when vmin-neighbor
                                     (vertex vmin vmin-neighbor #'plusp))
                                   (when vmax-neighbor
                                     (vertex vmax vmax-neighbor #'minusp))
                                   (setf e neighbor)
                                   (next :continue))
                                  ((< min (/ dt (- dt dh)))
                                   (setf min (/ dt (- dt dh)))
                                   (setf min-neighbor neighbor)
                                   (< max min))))
                           ((< dh 0)
                            (when (< (/ dt (- dt dh)) max)
                              (setf max (/ dt (- dt dh)))
                              (setf max-neighbor neighbor)
                              (< max min)))))))
          (when (or (side (edge-lplane e) (edge-left e))
                    (side (edge-rplane e) (edge-right e)))
            ;; e is outside the voronoi region
            (when (typep min-neighbor 'vertex)
              (vertex min min-neighbor #'>=)
              (setf e max-neighbor)
              (next :continue))
            (when (typep max-neighbor 'vertex)
              (vertex max max-neighbor #'<=)
              (setf e min-neighbor)
              (next :continue))
            (let* ((dt (plane-distance (face-plane min-neighbor) (tf-tail ce)))
                   (dh (plane-distance (face-plane min-neighbor) (tf-head ce)))
                   (dmin (+ dt (* min (- dh dt)))))
              (when (= 0 dmin)
                (setf cp (nv+ (v* (tf-segment ce) min)
                              (tf-tail ce)))
                (next :penetration))
              (setf e (if (< dmin 0)
                          (if (< dt dh) min-neighbor max-neighbor)
                          (if (< dh dt) min-neighbor max-neighbor)))
              (next :continue)))
          ;; edge intersects the voronoi region
          (etypecase min-neighbor
            (face
             (let* ((dt (plane-distance (face-plane min-neighbor) (tf-tail ce)))
                    (dh (plane-distance (face-plane min-neighbor) (tf-head ce)))
                    (dmin (+ dt (* min (- dh dt))))
                    (dmax (if max-neighbor (+ dt (* max (- dh dt))) dh)))
               (when (= 0 dmin)
                 (setf cp (nv+ (v* (tf-segment ce) min)
                               (tf-tail ce)))
                 (next :penetration))
               (when (or (and (< 0 dmin) (< dmin dmax))
                         (and (< dmin 0) (< dmax dmin)))
                 (setf e min-neighbor)
                 (next :continue))))
            (vertex
             (let ((p (nv- (nv+ (v* (tf-segment ce) min)
                                (tf-tail ce))
                           (vertex-location min-neighbor))))
               (when (v= 0 p)
                 (setf cp (vertex-location min-neighbor))
                 (next :penetration))
               (when (< 0 (v. p (tf-segment ce)))
                 (setf e min-neighbor)
                 (next :continue))))
            (null))
          ;; FIXME: might be able to deduplicate with above?
          (etypecase max-neighbor
            (face
             (let* ((dt (plane-distance (face-plane min-neighbor) (tf-tail ce)))
                    (dh (plane-distance (face-plane min-neighbor) (tf-head ce)))
                    (dmin (if min-neighbor (+ dt (* min (- dh dt))) dt))
                    (dmax (+ dt (* max (- dh dt)))))
               (when (= 0 dmin)
                 (setf cp (nv+ (v* (tf-segment ce) max)
                               (tf-tail ce)))
                 (next :penetration))
               (when (or (and (< 0 dmax) (< dmax dmin))
                         (and (< dmax 0) (< dmin dmax)))
                 (setf e max-neighbor)
                 (next :continue))))
            (vertex
             (let ((p (nv- (nv+ (v* (tf-segment ce) max)
                                (tf-tail ce))
                           (vertex-location max-neighbor))))
               (when (v= 0 p)
                 (setf cp (vertex-location max-neighbor))
                 (next :penetration))
               (when (< 0 (v. p (tf-segment ce)))
                 (setf e max-neighbor)
                 (next :continue))))
            (null))
          (next :disjoint))))))

(define-tester edge-edge (e1 e2 ce1 ce2 m12 m21)
  ;; Clip E1 against E2's cone
  (cache-edge m12 e1 ce1)
  (multiple-value-bind (result e2n cp2) (edge-edge-subtest e2 ce1 (state-b state))
    (setf e2 e2n)
    (v<- (state-b state) cp2)
    (when (eq result :penetration)
      (v<- (state-a state) cp2)
      (n*m m21 (state-a state)))
    (unless (eq result :disjoint)
      (finish result)))
  ;; Clip E2 against E1's cone
  (cache-edge m21 e2 ce2)
  (multiple-value-bind (result e1n cp1) (edge-edge-subtest e1 ce2 (state-a state))
    (setf e1 e1n)
    (v<- (state-a state) cp1)
    (when (eq result :penetration)
      (v<- (state-b state) cp1)
      (n*m m12 (state-b state)))
    (unless (eq result :disjoint)
      (finish result)))
  ;; We're disjoint on both, compute closest point.
  (let* ((dir (m* m21 (edge-direction e2)))
         (k (v. dir (edge-direction e1)))
         (h (v- (tf-tail ce2) (vertex-location (edge-tail e1))))
         (h2 (nv+ (v* dir (- k)) (edge-direction e1)))
         (num (v. h h2))
         (denom (- 1 (* k k))))
    (let* ((cp1 (if (= 0 denom)
                    (vertex-location (if (< 0 num) (edge-head e1) (edge-tail e1)))
                    (nv+ (v* (edge-direction e1) (trial:clamp 0 (/ num denom) (edge-length e1)))
                         (vertex-location (edge-tail e1)))))
           (coords (m* m12 cp1))
           (h (v- coords (vertex-location (edge-tail e2)))))
      (finish :disjoint 
              cp1
              (nv+ (v* (edge-direction e2) (v. h (edge-direction e2)))
                   (vertex-location (edge-tail e2)))))))

(define-tester edge-face (e f ce mef)
  (let ((code (make-array 3 :element-type T))
        (lam (make-array 3 :element-type 'single-float))
        (min 0) (max 1) min-cn max-cn chop-cn)
    (cache-edge mef e ce)
    (dotimes (i 3)
      (let* ((cni (aref (face-cone f) i))
             (dt (plane-distance (cone-plane cni) (tf-tail ce)))
             (dh (plane-distance (cone-plane cni) (tf-head ce))))
        (if (<= 0 dt)
            (cond ((<= 0 dh)
                   (setf (aref code i) :inside))
                  (T
                   (setf (aref code i) :max)
                   (when (< (/ dt (- dt dh)) max)
                     (setf max (/ dt (- dt dh)))
                     (setf max-cn cni))))
            (cond ((<= 0 dh)
                   (setf (aref code i) :min)
                   (when (< min (/ dt (- dt dh)))
                     (setf min (/ dt (- dt dh)))
                     (setf min-cn cni)))
                  (T
                   (setf (aref code i) :outside)
                   (setf chop-cn cni))))))
    (when (or chop-cn (< max min))
      (let ((next (or chop-cn (if (< 1 (+ min max)) min-cn max-cn))) prev cn intersected-p s)
        (loop until (eq next prev)
              for minv = NIL
              for maxv = NIL
              for i = (face-cone-index next)
              do (setf prev cn)
                 (setf cn next)
                 (setf s (cone-neighbor cn))
                 ;; Edge plane
                 (case (aref code i)
                   (:inside (return))
                   (:outside (setf min 0 max 1))
                   (:min (setf min 0 max (aref lam i)))
                   (:max (setf min (aref lam i) max 1)))
                 (block continue
                   (flet ((plane (plane v cw ccw)
                            (let ((dt (- (plane-distance plane (tf-tail ce))))
                                  (dh (- (plane-distance plane (tf-head ce)))))
                              (cond ((<= 0 dt)
                                     (when (and (< dh 0) (< (/ dt (- dt dh)) max))
                                       (setf max (/ dt (- dt dh)))
                                       (setf maxv v)
                                       (when (< max min)
                                         (when intersected-p (loop-finish))
                                         (setf next (if (eq (edge-left s) f) cw ccw))
                                         (return-from continue))))
                                    (T
                                     (when (< dh 0)
                                       (setf next (if (eq (edge-left s) f) cw ccw))
                                       (return-from continue))
                                     (when (< min (/ dt (- dt dh)))
                                       (setf min (/ dt (- dt dh)))
                                       (setf minv v)
                                       (when (< max min)
                                         (when intersected-p (loop-finish))
                                         (setf next (if (eq (edge-left s) f) cw ccw))
                                         (return-from continue))))))))
                     ;; Tail and Head planes
                     (plane (edge-tplane s) (edge-tail s) (face-cone-cw cn) (face-cone-ccw cn))
                     (plane (edge-hplane s) (edge-head s) (face-cone-ccw cn) (face-cone-cw cn)))
                   (setf intersected-p T)
                   (when minv
                     (let ((p (nv- (nv+ (v* (tf-segment ce) min)
                                        (tf-tail ce))
                                   (vertex-location minv))))
                       (when (< 0 (v. p (tf-segment ce)))
                         (setf next (if (eq f (edge-left s))
                                        (if (eq minv (edge-tail s)) (face-cone-cw cn) (face-cone-ccw cn))
                                        (if (eq minv (edge-tail s)) (face-cone-ccw cn) (face-cone-cw cn))))
                         (return-from continue))))
                   (when maxv
                     (let ((p (nv- (nv+ (v* (tf-segment ce) max)
                                        (tf-tail ce))
                                   (vertex-location maxv))))
                       (when (< (v. p (tf-segment ce)) 0)
                         (setf next (if (eq f (edge-left s))
                                        (if (eq maxv (edge-head s)) (face-cone-ccw cn) (face-cone-cw cn))
                                        (if (eq maxv (edge-head s)) (face-cone-cw cn) (face-cone-ccw cn))))
                         (return-from continue)))))
                 (setf f s)
                 (next))
        (setf f (if (eq prev (face-cone-ccw cn))
                    (if (eq f (edge-left f)) (edge-head s) (edge-tail s))
                    (if (eq f (edge-left f)) (edge-tail s) (edge-head s))))
        (next)))
    ;; Edge intersects faces cone
    (let* ((dt (- (plane-distance (face-plane f) (tf-tail ce))))
           (dh (- (plane-distance (face-plane f) (tf-head ce))))
           (dmin (if min-cn (+ dt (* min (- dh dt))) dt))
           (dmax (if max-cn (+ dt (* max (- dh dt))) dh)))
      (flet ((penetrate (length d-length)
               (finish :penetration
                       (nv+ (v* (edge-direction e) length (edge-length e))
                            (vertex-location (edge-tail e)))
                       (nv+* (nv+ (v* (tf-segment ce) length)
                                  (tf-tail ce))
                             (plane-normal (face-plane f)) (- d-length))
                       d-length)))
        (cond ((<= dmin 0)
               (when (<= 0 dmax)
                 (penetrate min dmin)))
              ((<= dmax 0)
               (penetrate max dmax))))
      ;; 
      (cond ((or (and (< 0 dmin) (<= dt dh))
                 (and (< dmin 0) (<= dh dt)))
             (cond (min-cn
                    (setf f (cone-neighbor min-cn)))
                   (T
                    (v<- (tf-location ce) (tf-tail ce))
                    (setf e (edge-tail e))
                    (setf (tf-feature ce) e))))
            (max-cn
             (setf f (cone-neighbor max-cn)))
            (T
             (v<- (tf-location ce) (tf-head ce))
             (setf e (edge-head e))
             (setf (tf-feature ce) e)))
      (next))))

(defun vclip-mesh (mesh1 mesh2 m12 m21 &key (max-iterations 5000) state)
  ;; FIXME: dunno if the A B initialisation here is ok.
  (let ((state (or state (make-state :a (m* m12 (vec 0 0 0))
                                     :b (m* m21 (vec 0 0 0))
                                     :lf (aref (mesh-vertices mesh1) 0)
                                     :rf (aref (mesh-vertices mesh2) 0))))
        (cf1 (make-transform-cache))
        (cf2 (make-transform-cache)))
    (dotimes (i max-iterations (values state NIL))
      (flet ((swap ()
               (rotatef (state-a state) (state-b state))
               (rotatef (state-lf state) (state-rf state)))
             (test (fun &rest args)
               (let ((result (apply fun state args)))
                 (unless (eq result :continue)
                   (return (values state result))))))
        (etypecase (state-lf state)
          (vertex
           (etypecase (state-rf state)
             (vertex (test #'vertex-vertex cf1 cf2 m12 m21))
             (edge (test #'vertex-edge cf1 cf2 m12 m21))
             (face (test #'vertex-face cf1 cf2 m12 m21 mesh2))))
          (edge
           (etypecase (state-rf state)
             (vertex (swap) (test #'vertex-edge cf2 cf1 m21 m12))
             (edge (test #'edge-edge cf1 cf2 m12 m21))
             (face (test #'edge-face cf1 cf2 m12 m21))))
          (face
           (etypecase (state-rf state)
             (vertex (swap) (test #'vertex-face cf2 cf1 m21 m12 mesh1))
             (edge (swap) (test #'edge-face cf2 cf1 m21 m12)))))))))

