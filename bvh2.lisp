(defpackage #:org.shirakumo.fraf.trial.bvh2
  (:use #:cl #:3d-vectors)
  (:import-from #:org.shirakumo.fraf.trial #:location #:bsize)
  (:export
   #:bvh
   #:make-bvh
   #:bvh-insert
   #:bvh-remove
   #:bvh-update
   #:bvh-check
   #:bvh-print
   #:bvh-lines
   #:bvh-reinsert-all
   #:call-with-contained
   #:call-with-overlapping
   #:do-fitting))

(in-package #:org.shirakumo.fraf.trial.bvh2)

;; CF https://www.researchgate.net/publication/254007711_Fast_Effective_BVH_Updates_for_Animated_Scenes

(defstruct (bvh-node
            (:include vec4)
            (:constructor %make-bvh-node (3d-vectors::%vx4 3d-vectors::%vy4 3d-vectors::%vz4 3d-vectors::%vw4 d p l r o))
            (:copier NIL)
            (:predicate NIL))
  (d 0 :type (unsigned-byte 16))
  (p NIL :type (or null bvh-node))
  (l NIL :type (or null bvh-node))
  (r NIL :type (or null bvh-node))
  (o NIL :type T))

(defmethod print-object ((node bvh-node) stream)
  (print-unreadable-object (node stream :type T)
    (let ((o (when (bvh-node-o node) (princ-to-string (bvh-node-o node)))))
      (format stream "~f ~f ~f ~f ~d~@[ ~a~]" (vx4 node) (vy4 node) (vz4 node) (vw4 node) (bvh-node-d node) o))))

(defun make-bvh-node-for (object parent)
  (let ((loc (location object))
        (siz (bsize object)))
    (%make-bvh-node (- (vx loc) (vx siz))
                    (- (vy loc) (vy siz))
                    (+ (vx loc) (vx siz))
                    (+ (vy loc) (vy siz))
                    (1+ (bvh-node-d parent))
                    parent NIL NIL object)))

(defun node-refit-object (node object)
  (let ((x (vx4 node)) (y (vy4 node))
        (z (vz4 node)) (w (vw4 node))
        (loc (location object))
        (siz (bsize object)))
    (setf (vx4 node) (- (vx loc) (vx siz)))
    (setf (vy4 node) (- (vy loc) (vy siz)))
    (setf (vz4 node) (+ (vx loc) (vx siz)))
    (setf (vw4 node) (+ (vy loc) (vy siz)))
    (when (or (/= x (vx4 node))
              (/= y (vy4 node))
              (/= z (vz4 node))
              (/= w (vw4 node)))
      (when (bvh-node-p node)
        (node-refit (bvh-node-p node)))
      T)))

(defun node-fit (node l r)
  (setf (vx4 node) (min (vx4 l) (vx4 r)))
  (setf (vy4 node) (min (vy4 l) (vy4 r)))
  (setf (vz4 node) (max (vz4 l) (vz4 r)))
  (setf (vw4 node) (max (vw4 l) (vw4 r))))

(defun node-refit (node)
  (let ((x (vx4 node)) (y (vy4 node))
        (z (vz4 node)) (w (vw4 node)))
    (node-fit node (bvh-node-l node) (bvh-node-r node))
    (when (or (/= x (vx4 node))
              (/= y (vy4 node))
              (/= z (vz4 node))
              (/= w (vw4 node)))
      (when (bvh-node-p node)
        (node-refit (bvh-node-p node)))
      T)))

(defun node-split (node object)
  (let ((l (make-bvh-node-for (bvh-node-o node) node))
        (r (make-bvh-node-for object node)))
    (setf (bvh-node-o node) NIL)
    (setf (bvh-node-l node) l)
    (setf (bvh-node-r node) r)
    (node-refit node)
    node))

(declaim (inline node-contains-p*))
(defun node-contains-p* (node loc siz)
  (declare (optimize speed (safety 0)))
  (declare (type vec4 node))
  (declare (type vec2 loc siz))
  (let ((nx (vx4 node))
        (ny (vy4 node))
        (nz (vz4 node))
        (nw (vw4 node))
        (lx (vx2 loc))
        (ly (vy2 loc))
        (sx (vx2 siz))
        (sy (vy2 siz)))
    (and (<= nx (+ lx sx))
         (<= ny (+ ly sy))
         (<= (- lx sx) nz)
         (<= (- ly sy) nw))))

(defun node-contains-p (node object)
  (declare (optimize speed (safety 0)))
  (declare (type vec4 node))
  (let ((loc (location object))
        (siz (bsize object)))
    (declare (type vec2 loc siz))
    (node-contains-p* node loc siz)))

(declaim (inline node-overlaps-p))
(defun node-overlaps-p (node region)
  (declare (optimize speed (safety 0)))
  (declare (type vec4 node region))
  (let ((nx (vx4 node))
        (ny (vy4 node))
        (nz (vz4 node))
        (nw (vw4 node))
        (rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region)))
    (and (<= nx rz)
         (<= ny rw)
         (<= rx nz)
         (<= ry nw))))

(defun node-sub-p (node sub)
  (declare (optimize speed))
  (and (<= (vx4 node) (vx4 sub))
       (<= (vy4 node) (vy4 sub))
       (<= (vz4 sub) (vz4 node))
       (<= (vw4 sub) (vw4 node))))

(defun better-fit (a b object)
  (declare (optimize speed))
  (let ((ca (node-contains-p a object))
        (cb (node-contains-p b object)))
    (cond ((eq ca cb)
           ;; If it's in neither or in both, just see whose centroid we're closer to.
           (let ((ax (/ (+ (vz4 a) (vx4 a)) 2.0))
                 (ay (/ (+ (vw4 a) (vy4 a)) 2.0))
                 (bx (/ (+ (vz4 b) (vx4 b)) 2.0))
                 (by (/ (+ (vw4 b) (vy4 b)) 2.0))
                 (loc (location object)))
             (if (< (+ (expt (- ax (vx loc)) 2)
                       (expt (- ay (vy loc)) 2))
                    (+ (expt (- bx (vx loc)) 2)
                       (expt (- by (vy loc)) 2)))
                 a b)))
          (ca a)
          (cb b))))

(defun node-insert (node object)
  (declare (optimize speed))
  (cond ((bvh-node-o node)
         (node-split node object))
        ((bvh-node-r node)
         (node-insert (better-fit (bvh-node-l node) (bvh-node-r node) object) object))
        (T
         (setf (bvh-node-o node) object)
         (node-refit-object node object)
         node)))

(defun node-sibling (node)
  (declare (optimize speed))
  (let* ((p (bvh-node-p node))
         (l (bvh-node-l p))
         (r (bvh-node-r p)))
    (cond ((eq node l) r)
          ((eq node r) l)
          (T (error "What the fuck?")))))

(defun set-depth (node d)
  (declare (optimize speed))
  (setf (bvh-node-d node) d)
  (unless (bvh-node-o node)
    (set-depth (bvh-node-l node) (1+ d))
    (set-depth (bvh-node-r node) (1+ d))))

(defun node-transfer (target source)
  (declare (optimize speed))
  (setf (vx4 target) (vx4 source))
  (setf (vy4 target) (vy4 source))
  (setf (vz4 target) (vz4 source))
  (setf (vw4 target) (vw4 source))
  (let ((l (bvh-node-l source))
        (r (bvh-node-r source)))
    (setf (bvh-node-l target) l)
    (setf (bvh-node-r target) r)
    (when l
      (setf (bvh-node-p l) target)
      (setf (bvh-node-p r) target))
    (setf (bvh-node-o target) (bvh-node-o source)))
  (set-depth target (bvh-node-d source)))

(defun node-remove (node)
  (declare (optimize speed))
  (let ((p (bvh-node-p node)))
    (cond (p
           (node-transfer p (node-sibling node))
           (when (bvh-node-p p)
             (node-refit (bvh-node-p p)))
           p)
          (T
           (setf (bvh-node-o node) NIL)
           node))))

(defstruct (bvh
            (:constructor make-bvh ())
            (:copier NIL)
            (:predicate NIL))
  (root (%make-bvh-node 0f0 0f0 0f0 0f0 0 NIL NIL NIL NIL) :type bvh-node)
  (table (make-hash-table :test 'eq) :type hash-table))

(defun bvh-insert (bvh object)
  (declare (optimize speed))
  (let ((node (node-insert (bvh-root bvh) object))
        (table (bvh-table bvh)))
    (cond ((eq object (bvh-node-o node))
           (setf (gethash object table) node))
          (T
           (setf (gethash (bvh-node-o (bvh-node-l node)) table) (bvh-node-l node))
           (setf (gethash (bvh-node-o (bvh-node-r node)) table) (bvh-node-r node))))
    object))

(defun bvh-remove (bvh object)
  (declare (optimize speed))
  (let* ((table (bvh-table bvh))
         (node (gethash object table)))
    (when node
      (remhash object table)
      (let ((p (node-remove node)))
        (setf (gethash (bvh-node-o p) table) p)))))

(defun bvh-update (bvh object)
  (declare (optimize speed))
  ;; FIXME: Figure out when to rebalance the tree.
  (let ((node (gethash object (bvh-table bvh))))
    (when node
      (node-refit-object node object))))

(defmethod trial:enter (object (bvh bvh))
  (bvh-insert bvh object))

(defmethod trial:leave (object (bvh bvh))
  (bvh-remove bvh object))

(defmethod trial::clear ((bvh bvh))
  (clrhash (bvh-table bvh))
  (setf (bvh-root bvh) (%make-bvh-node 0f0 0f0 0f0 0f0 0 NIL NIL NIL NIL))
  bvh)

(defun bvh-print (bvh)
  (format T "~&-------------------------")
  (labels ((recurse (node)
             (format T "~&~v@{|  ~}└ ~a" (bvh-node-d node) node)
             (unless (bvh-node-o node)
               (recurse (bvh-node-l node))
               (recurse (bvh-node-r node)))))
    (recurse (bvh-root bvh))))

(defun bvh-lines (bvh)
  (let ((p ()))
    (labels ((depth-color (depth)
               (let ((d (max 0.0 (- 1.0 (/ depth 100)))))
                 (vec 1 d d 0.1)))
             (recurse (node)
               (let ((color (depth-color (bvh-node-d node))))
                 (push (list (vxy_ node) color) p)
                 (push (list (vzy_ node) color) p)
                 (push (list (vxw_ node) color) p)
                 (push (list (vzw_ node) color) p)
                 (push (list (vxy_ node) color) p)
                 (push (list (vxw_ node) color) p)
                 (push (list (vzy_ node) color) p)
                 (push (list (vzw_ node) color) p)
                 (when (bvh-node-l node)
                   (recurse (bvh-node-l node))
                   (recurse (bvh-node-r node))))))
      (recurse (bvh-root bvh)))
    p))

(defun bvh-check (bvh)
  (labels ((recurse (node)
             (cond ((bvh-node-l node)
                    (unless (eq node (bvh-node-p (bvh-node-l node)))
                      (error "The left child~%  ~a~%is not parented to~%  ~a"
                             (bvh-node-l node) node))
                    (unless (eq node (bvh-node-p (bvh-node-r node)))
                      (error "The right child~%  ~a~%is not parented to~%  ~a"
                             (bvh-node-r node) node))
                    (unless (node-sub-p node (bvh-node-l node))
                      (error "The parent node~%  ~a~%does not contain the left child~%  ~a"
                             node (bvh-node-l node)))
                    (unless (node-sub-p node (bvh-node-r node))
                      (error "The parent node~%  ~a~%does not contain the right child~%  ~a"
                             node (bvh-node-r node)))
                    (recurse (bvh-node-l node))
                    (recurse (bvh-node-r node)))
                   ((bvh-node-o node)
                    (unless (eq node (gethash (bvh-node-o node) (bvh-table bvh)))
                      (error "The node~%  ~a~%is not assigned to object~%  ~a~%as it is assigned to~%  ~a"
                             node (bvh-node-o node) (gethash (bvh-node-o node) (bvh-table bvh))))))))
    (recurse (bvh-root bvh)))
  (loop for o being the hash-keys of (bvh-table bvh)
        for n being the hash-values of (bvh-table bvh)
        do (unless (eq o (bvh-node-o n))
             (error "The node~%  ~a~%does not refer to object~%  ~a~%and instead tracks~%  ~a"
                    n o (bvh-node-o n)))))

(defun bvh-refit (bvh)
  (declare (optimize speed))
  (labels ((recurse (node)
             (cond ((bvh-node-l node)
                    (recurse (bvh-node-l node))
                    (recurse (bvh-node-r node))
                    (node-refit node))
                   (T
                    (node-refit-object node (bvh-node-o node))))))
    (recurse (bvh-root bvh))))

(defun call-with-contained (function bvh region)
  (declare (optimize speed (safety 1)))
  (let ((function (etypecase function
                    (symbol (fdefinition function))
                    (function function)))
        (tentative (make-array 128))
        (i 0))
    (declare (type (integer 0 128) i))
    (declare (dynamic-extent tentative))
    (flet ((add (node)
             (setf (svref tentative i) node)
             (incf i)))
      (declare (inline add))
      (add (bvh-root bvh))
      (loop (decf i)
            (let ((node (svref tentative i)))
              (when (node-overlaps-p node region)
                (let ((o (bvh-node-o node)))
                  (cond (o
                         (funcall function o))
                        (T
                         (add (bvh-node-l node))
                         (add (bvh-node-r node))))))
              (when (= 0 i)
                (return)))))))

(defun call-with-overlapping (function bvh object)
  (declare (optimize speed (safety 1)))
  (flet ((ensure-vec2 (x)
           (etypecase x
             (vec2 x)
             (vec3 (vsetf (load-time-value (vec 0 0))
                          (vx3 x) (vy3 x)))
             (vec4 (vsetf (load-time-value (vec 0 0 0 0))
                          (vx4 x) (vy4 x))))))
    (let ((function (etypecase function
                      (symbol (fdefinition function))
                      (function function)))
          (loc (ensure-vec2 (location object)))
          (siz (ensure-vec2 (bsize object))))
      (labels ((recurse (node)
                 (when (node-contains-p* node loc siz)
                   (let ((o (bvh-node-o node)))
                     (cond (o
                            (funcall function o))
                           (T
                            (recurse (bvh-node-l node))
                            (recurse (bvh-node-r node))))))))
        (recurse (bvh-root bvh))))))

(defmacro do-fitting ((entity bvh region &optional result) &body body)
  ;; REGION should be a vec2 for a point test, or a vec4 with left/bottom/right/top coordinates.
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(block NIL
       (flet ((,thunk (,entity)
                ,@body))
         (declare (dynamic-extent #',thunk))
         (let ((,regiong ,region))
           (etypecase ,regiong
             (vec2 (let ((,regiong (3d-vectors::%vec4 (vx2 ,regiong) (vy2 ,regiong) (vx2 ,regiong) (vy2 ,regiong))))
                     (declare (dynamic-extent ,regiong))
                     (call-with-contained #',thunk ,bvh ,regiong)))
             (vec4 (call-with-contained #',thunk ,bvh ,regiong))
             (trial:entity (call-with-overlapping #',thunk ,bvh ,regiong)))))
       ,result)))

(defstruct (bvh-iterator
            (:constructor make-bvh-iterator (bvh region))
            (:copier NIL)
            (:predicate NIL))
  (bvh NIL :type bvh)
  (region NIL :type vec4))

(defmethod for:make-iterator ((bvh bvh) &key in)
  (if in
      (make-bvh-iterator bvh in)
      bvh))

(defmethod for:step-functions ((iterator bvh-iterator))
  (declare (optimize speed))
  (let ((node (bvh-root (bvh-iterator-bvh iterator)))
        (region (bvh-iterator-region iterator)))
    (labels ((next-leaf (node child)
               (when node
                 (let ((l (bvh-node-l node))
                       (r (bvh-node-r node)))
                   (cond ((bvh-node-o node)
                          node)
                         ((null child)
                          (if (node-overlaps-p l region)
                              (next-leaf l NIL)
                              (next-leaf node l)))
                         ((eq child l)
                          (if (node-overlaps-p r region)
                              (next-leaf r NIL)
                              (next-leaf node r)))
                         ((eq child r)
                          (next-leaf (bvh-node-p node) node)))))))
      (setf node (next-leaf node NIL))
      (values
       (lambda ()
         (prog1 (bvh-node-o node)
           (setf node (next-leaf (bvh-node-p node) node))))
       (lambda ()
         node)
       (lambda (value)
         (declare (ignore value))
         (error "Not supported"))
       (lambda ())))))

(defmethod for:step-functions ((bvh bvh))
  (declare (optimize speed))
  (let ((node (bvh-root bvh)))
    (labels ((next-leaf (node child)
               (when node
                 (cond ((bvh-node-o node)
                        node)
                       ((null child)
                        (next-leaf (bvh-node-l node) NIL))
                       ((eq child (bvh-node-l node))
                        (next-leaf (bvh-node-r node) NIL))
                       ((eq child (bvh-node-r node))
                        (next-leaf (bvh-node-p node) node))))))
      (setf node (next-leaf node NIL))
      (values
       (lambda ()
         (prog1 (bvh-node-o node)
           (setf node (next-leaf (bvh-node-p node) node))))
       (lambda ()
         node)
       (lambda (value)
         (declare (ignore value))
         (error "Not supported"))
       (lambda ())))))

(progn
  (declaim (inline perimiter-heuristic cost node-cost node-priority))

  (defun perimiter-heuristic (x y z w)
    (declare (optimize speed (safety 0))
             (type single-float x y z w))
    (+ (- z x)
       (- w y)))

  (defun cost (x y z w)
    (declare (optimize speed (safety 0)))
    (the single-float
         (perimiter-heuristic x y z w)))

  (defun node-cost (node)
    (declare (optimize speed (safety 0)) (type vec4 node))
    (the single-float
         (cost (vx4 node) (vy4 node)
               (vz4 node) (vw4 node))))

  (defun node-priority (node)
    (declare (optimize speed (safety 0)))
    (if (bvh-node-p node)
        (/ (node-cost (bvh-node-p node))
           (node-cost node))
        0.0)))

;; like "Parallel Reinsertion for Bounding Volume Hierarchy Optimization" (D. Meister and J. Bittner)
;; the paper is mostly about optimization for paralellism, with the algorithm being derived from
;; "Fast Insertion-Based Optimization of Bounding Volume Hierarchies" (J. Bittner, M. Hapala, and V. Havran)
(defun node-find-best-reinsertion-position (node bvh)
  "Returns the node for which NODE being reinserted as its child would
create the greatest cumulative decrease in box perimiter."
  (declare (optimize speed))
  (when (bvh-node-p node)
    (let* ((parent (bvh-node-p node))
           (pivot parent)
           (pivot-box (vec4 0.0 0.0 0.0 0.0))
           (pivot-box-initialized nil)
           (decrease-bound (- (node-cost parent) (node-cost node)))
           (decrease 0.0)
           (decrease-best decrease)
           (out (node-sibling node))
           (out-best out)
           (down t))
      (declare (dynamic-extent pivot-box)
               (type single-float decrease-bound decrease best-decrease))
      (flet ((union-cost (node-a node-b)
               (declare (optimize speed (safety 0)))
               (cost (min (vx4 node-a)
                          (vx4 node-b))
                     (min (vy4 node-a)
                          (vy4 node-b))
                     (max (vz4 node-a)
                          (vz4 node-b))
                     (max (vw4 node-a)
                          (vw4 node-b)))))
        (declare (inline union-cost))
        (loop (if down
                  ;; traverse downwards
                  (let* ((merged-cost (union-cost node out))
                         ;; how much better off we would be if OUT was NODE's parent
                         (decrease-direct (- (node-cost parent)
                                             merged-cost)))
                    (when (< decrease-best (+ decrease decrease-direct))
                      (setq decrease-best (+ decrease decrease-direct))
                      (setq out-best out))
                    ;; decrease of cost gets worse as you descend
                    (decf decrease (- merged-cost (node-cost out)))
                    (if (or (bvh-node-o out) ; stop at leaves
                            (<= (+ decrease-bound decrease)
                                ;; early stop heuristic; if descending
                                ;; would be worse than where we started,
                                ;; stop descending.
                                decrease-best))
                        (setq down nil)
                        (setq out (bvh-node-l out))))
                  (progn    ; traverse upwards towards pivot
                    ;; decrease of cost improves as you ascend
                    (incf decrease (- (union-cost node out) (node-cost out)))
                    (if (eq pivot (bvh-node-p out))
                        (progn   ; we've reached the pivot
                          (if pivot-box-initialized ; expand the pivot box to include OUT
                              (node-fit pivot-box pivot-box out)
                              (progn
                                (vsetf pivot-box (vx out) (vy out) (vz out) (vw out))
                                (setq pivot-box-initialized t)))
                          (setq out (bvh-node-p out)) ; consider the pivot as OUT,
                          (when (not (eq out parent)) ; but not if we're still on the original parent.
                            ;; how much better off we would be if PIVOT-BOX was NODE's parent
                            (let ((decrease-direct (- (node-cost parent)
                                                      (union-cost node pivot-box))))
                              (when (< decrease-best (+ decrease decrease-direct))
                                (setq decrease-best (+ decrease decrease-direct))
                                ;; set pivot as best
                                (setq out-best out)))
                            (decf decrease (- (node-cost pivot-box)
                                              (node-cost out))))
                          (when (eq out (bvh-root bvh)) ; stop at root
                            ;; If the best position found is the
                            ;; sibling, NODE is already in the best spot.
                            (return (unless (eq out-best (node-sibling node))
                                      out-best)))
                          ;; when we haven't stopped, move the pivot up
                          ;; and traverse down on the sibling branch
                          (setq out (node-sibling pivot))
                          (setq pivot (bvh-node-p out))
                          (setq down t))

                        ;; we haven't reached the pivot. traverse sibling down, or parent up
                        (if (eq out (bvh-node-l (bvh-node-p out)))
                            (progn
                              (setq down t)
                              (setq out (node-sibling out)))
                            (setq out (bvh-node-p out)))))))))))

(defun node-remove-and-reinsert (bvh node out)
  "Remove NODE from BVH, then splice it back into the tree so that OUT is its parent."
  (declare (optimize speed))
  (let ((sibling (node-sibling node))
        (parent (bvh-node-p node)))
    (when parent
      ;; Put SIBLING where PARENT is
      (let ((grandparent (bvh-node-p parent)))
        (if grandparent
            (progn
              (if (eq (bvh-node-l grandparent) parent)
                  (setf (bvh-node-l grandparent) sibling)
                  (setf (bvh-node-r grandparent) sibling))
              (setf (bvh-node-p sibling) grandparent))
            ;; if there's no grandparent, parent is the root.
            (progn (setf (bvh-root bvh) sibling)
                   (setf (bvh-node-p sibling) nil)))
        (set-depth sibling (1- (bvh-node-d sibling)))
        (when grandparent
          (node-refit grandparent)))
      ;; Put PARENT where OUT is, with NODE and OUT as its children
      (let ((grandparent (bvh-node-p out)))
        (if grandparent
            (progn
              (if (eq (bvh-node-l grandparent) out)
                  (setf (bvh-node-l grandparent) parent)
                  (setf (bvh-node-r grandparent) parent))
              (setf (bvh-node-p parent) grandparent))
            ;; out is the root
            (progn (setf (bvh-root bvh) parent)
                   (setf (bvh-node-p parent) nil)))
        (setf (bvh-node-r parent) out)
        (setf (bvh-node-l parent) node)
        (setf (bvh-node-p out) parent)
        (setf (bvh-node-p node) parent)
        (set-depth parent (bvh-node-d out))
        (node-refit parent)))))

(defun node-reinsert-to-best (node bvh)
  "Remove the specified node from the bvh and reinsert it into its
optimal position in the tree."
  (let ((best (node-find-best-reinsertion-position node bvh)))
    (when best
      (node-remove-and-reinsert bvh node best)
      t)))

(defun bvh-reinsert-all (bvh &optional (rounds 1))
  "Reinsert each node in BVH to its optimal position in the
tree. Results may improve with multiple passes, the number of which
may be specified by ROUNDS."
  (labels ((descend (node)
             (node-reinsert-to-best node bvh)
             (unless (bvh-node-o node)
               (descend (bvh-node-l node))
               (descend (bvh-node-r node))))
           (bvh-empty-p (bvh)
             (let ((root (bvh-root bvh)))
               (not (or (bvh-node-o root)
                        (bvh-node-l root)
                        (bvh-node-r root))))))
    (unless (bvh-empty-p bvh)
      (descend (bvh-root bvh))
      (when (> rounds 0) (bvh-reinsert-all bvh (1- rounds))))))
