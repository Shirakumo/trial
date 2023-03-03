(in-package #:org.shirakumo.fraf.trial.quadtree)

(defstruct (quadtree-node
            (:include vec4) ;; x and y are top left corner, z and w are bottom right corner.
            (:constructor %make-quadtree-node
                (3d-vectors::%vx4 3d-vectors::%vy4 3d-vectors::%vz4 3d-vectors::%vw4
                 depth min-size threshold parent top-left top-right bottom-left bottom-right))
            (:copier NIL)
            (:predicate NIL))
  (depth 0 :type (unsigned-byte 32)) ;; For debugging.
  (min-size 1 :type (unsigned-byte 16)) ;; Minimum quad size.
  (threshold 1 :type (unsigned-byte 16)) ;; Number of objects in a quad before it's split.
  (parent NIL :type (or null quadtree-node))
  (top-left NIL :type (or null quadtree-node)) ;; Child quads.
  (top-right NIL :type (or null quadtree-node))
  (bottom-left NIL :type (or null quadtree-node))
  (bottom-right NIL :type (or null quadtree-node))
  (active-p NIL :type boolean) ;; We disable nodes when they have no content for searches.
  (objects (make-array 4 :adjustable T :fill-pointer 0) :type (vector T))) ;; Stays very short.

(defmethod print-object ((node quadtree-node) stream)
  (print-unreadable-object (node stream :type T)
    (with-vec4 (x y z w) node
      (format stream "~a ~a ~a ~a~@[: ~{~a~^, ~}~]" x y z w (node-list-objects node)))))

(declaim (inline node-empty-p))
(defun node-empty-p (node)
  (declare (optimize speed))
  (= 0 (length (quadtree-node-objects node))))

(defun node-list-objects (node)
  (loop for object across (quadtree-node-objects node) collecting object))

(defun node-pop-objects (node)
  (declare (optimize speed))
  (loop until (node-empty-p node)
        collect (vector-pop (quadtree-node-objects node))))

(defmacro with-node-children ((top-left top-right bottom-left bottom-right) node &body body)
  `(let ((,top-left (quadtree-node-top-left ,node))
         (,top-right (quadtree-node-top-right ,node))
         (,bottom-left (quadtree-node-bottom-left ,node))
         (,bottom-right (quadtree-node-bottom-right ,node)))
     ,@body))

(declaim (inline node-active-p))
(defun node-active-p (node)
  (declare (optimize speed))
  (and node (quadtree-node-active-p node)))

(defun node-active-children-p (node)
  (declare (optimize speed))
  (with-node-children (tl tr bl br) node
    (or (node-active-p tl)
        (node-active-p tr)
        (node-active-p bl)
        (node-active-p br))))

(defun node-children (node)
  (declare (optimize speed))
  (with-node-children (tl tr bl br) node
    (let ((children ()))
      (when br (push br children))
      (when bl (push bl children))
      (when tr (push tr children))
      (when tl (push tl children))
      children)))

(defun node-active-children (node)
  (declare (optimize speed))
  (with-node-children (tl tr bl br) node
    (let ((children ()))
      (when (node-active-p br) (push br children))
      (when (node-active-p bl) (push bl children))
      (when (node-active-p tr) (push tr children))
      (when (node-active-p tl) (push tl children))
      children)))

(defun ensure-child-nodes (node)
  (declare (optimize speed))
  (with-vec4 (left top right bottom) node
    (let ((depth (1+ (quadtree-node-depth node)))
          (min-size (quadtree-node-min-size node))
          (threshold (quadtree-node-threshold node))
          (mid-x (+ left (/ (- right left) 2f0)))
          (mid-y (+ top (/ (- bottom top) 2f0))))
      (unless (quadtree-node-top-left node)
        (setf (quadtree-node-top-left node) (%make-quadtree-node
                                             left top mid-x mid-y depth min-size threshold
                                             node NIL NIL NIL NIL)))
      (unless (quadtree-node-top-right node)
        (setf (quadtree-node-top-right node) (%make-quadtree-node
                                              mid-x top right mid-y depth min-size threshold
                                              node NIL NIL NIL NIL)))
      (unless (quadtree-node-bottom-left node)
        (setf (quadtree-node-bottom-left node) (%make-quadtree-node
                                                left mid-y mid-x bottom depth min-size threshold
                                                node NIL NIL NIL NIL)))
      (unless (quadtree-node-bottom-right node)
        (setf (quadtree-node-bottom-right node) (%make-quadtree-node
                                                 mid-x mid-y right bottom depth min-size threshold
                                                 node NIL NIL NIL NIL))))))

(defun node-remove-children (node &key recurse)
  (declare (optimize speed))
  (let ((children (node-children node)))
    (setf (quadtree-node-top-left node) NIL)
    (setf (quadtree-node-top-right node) NIL)
    (setf (quadtree-node-bottom-left node) NIL)
    (setf (quadtree-node-bottom-right node) NIL)
    (loop for child in children
          do (setf (quadtree-node-parent child) NIL)
          when recurse do (node-remove-children child :recurse T))))

(declaim (inline region-contains-p))
(defun region-contains-p (region other)
  (declare (optimize speed))
  (declare (type vec4 region other))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (ox (vx4 other))
        (oy (vy4 other))
        (oz (vz4 other))
        (ow (vw4 other)))
    (and (<= rx ox) ;; Contains completely.
         (<= ry oy)
         (<= oz rz)
         (<= ow rw))))

(declaim (inline region-contains-object-p*))
(defun region-contains-object-p* (region loc siz)
  (declare (optimize speed))
  (declare (type vec4 region))
  (declare (type vec2 loc siz))
  (let ((nx (vx4 region))
        (ny (vy4 region))
        (nz (vz4 region))
        (nw (vw4 region))
        (lx (vx2 loc))
        (ly (vy2 loc))
        (sx (vx2 siz))
        (sy (vy2 siz)))
    (and (<= nx lx) ;; Contains completely.
         (<= ny ly)
         (<= (+ lx sx) nz)
         (<= (+ ly sy) nw))))

(defun region-contains-object-p (region object)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((loc (location object))
        (siz (bsize object)))
    (declare (type vec2 loc siz))
    (region-contains-object-p* region loc siz)))

(declaim (inline region-overlaps-p))
(defun region-overlaps-p (region other)
  (declare (optimize speed))
  (declare (type vec4 region other))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (ox (vx4 other))
        (oy (vy4 other))
        (oz (vz4 other))
        (ow (vw4 other)))
    (and (<= rx oz) ;; Partial touch.
         (<= ox rz)
         (<= ry ow)
         (<= oy rw))))

(declaim (inline region-overlaps-with-p*))
(defun region-overlaps-with-p* (region loc siz)
  (declare (optimize speed))
  (declare (type vec4 region))
  (declare (type vec2 loc siz))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (lx (vx2 loc))
        (ly (vy2 loc))
        (sx (vx2 siz))
        (sy (vy2 siz)))
    (and (<= rx (+ lx sx)) ;; Partial touch.
         (<= lx rz)
         (<= ry (+ ly sy))
         (<= ly rw))))

(defun region-overlaps-with-p (region object)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((loc (location object))
        (siz (bsize object)))
    (declare (type vec2 loc siz))
    (region-overlaps-with-p* region loc siz)))

(defun node-split (node table)
  (declare (optimize speed))
  ;; Do not split if the node is not active, it's empty, it'd split the node below the minimum size,
  ;; or there are no active children while we are still below the threshold.
  (when (and (quadtree-node-active-p node)
             (not (node-empty-p node))
             (<= (* 2f0 (quadtree-node-min-size node)) (- (vz4 node) (vx4 node)))
             (<= (* 2f0 (quadtree-node-min-size node)) (- (vw4 node) (vy4 node)))
             (or (node-active-children-p node)
                 (< (quadtree-node-threshold node)
                    (length (quadtree-node-objects node)))))
    (let ((objects (node-pop-objects node)))
      ;; Clear and rearrange the objects.
      (ensure-child-nodes node)
      (with-node-children (tl tr bl br) node
        (loop for object in objects
              for match = (cond
                            ((region-contains-object-p tl object) tl)
                            ((region-contains-object-p tr object) tr)
                            ((region-contains-object-p bl object) bl)
                            ((region-contains-object-p br object) br)
                            (T node))
              do (setf (gethash object table) match)
              do (setf (quadtree-node-active-p match) T)
              do (vector-push-extend object (quadtree-node-objects match))
              unless (eq match node) do (node-split match table)))))
  node)

(defun node-increase-depth (node)
  (declare (optimize speed))
  (incf (quadtree-node-depth node))
  (loop for child in (node-children node)
        do (node-increase-depth child)))

(declaim (inline node-direction*))
(defun node-direction* (node loc)
  (declare (optimize speed))
  (declare (type vec4 node))
  (declare (type vec2 loc))
  (let ((nx (vx4 node))
        (ny (vy4 node))
        (lx (vx2 loc))
        (ly (vy2 loc)))
    (if (< lx nx)
        (if (< ly ny) :top-left :bottom-left)
        (if (< ly ny) :top-right :bottom-right))))

(defun node-direction (node object)
  (declare (optimize speed))
  (declare (type vec4 node))
  (let ((loc (location object)))
    (declare (type vec2 loc))
    (node-direction* node loc)))

(defun node-extend (node direction)
  (declare (optimize speed))
  (with-vec4 (node-x node-y node-z node-w) node
    (let ((min-size (quadtree-node-min-size node))
          (threshold (quadtree-node-threshold node))
          (width (- node-z node-x))
          (height (- node-w node-y)))
      (flet ((child (x y z w)
               (%make-quadtree-node x y z w 1 min-size threshold NIL NIL NIL NIL NIL)))
        (multiple-value-bind (x y z w)
            (ecase direction
              (:bottom-right (values node-x node-y (+ node-z width) (+ node-w height)))
              (:bottom-left (values (- node-x width) node-y node-z (+ node-w height)))
              (:top-right (values node-x (- node-y height) (+ node-z width) node-w))
              (:top-left (values (- node-x width) (- node-y height) node-z node-w)))
          (let* ((mid-x (+ x width))
                 (mid-y (+ y height))
                 (top-left (if (eql direction :bottom-right) node (child x y mid-x mid-y)))
                 (top-right (if (eql direction :bottom-left) node (child mid-x y z mid-y)))
                 (bottom-left (if (eql direction :top-right) node (child x mid-y mid-x w)))
                 (bottom-right (if (eql direction :top-left) node (child mid-x mid-y z w)))
                 (parent (%make-quadtree-node
                          x y z w 0 min-size threshold NIL
                          top-left top-right bottom-left bottom-right)))
            (setf (quadtree-node-parent top-left) parent)
            (setf (quadtree-node-parent top-right) parent)
            (setf (quadtree-node-parent bottom-left) parent)
            (setf (quadtree-node-parent bottom-right) parent)
            (setf (quadtree-node-active-p parent) (quadtree-node-active-p node))
            (node-increase-depth node)
            parent))))))

(defun node-insert (node object table)
  (declare (optimize speed))
  (when (region-contains-object-p node object)
    (setf (gethash object table) node)
    (setf (quadtree-node-active-p node) T)
    (vector-push-extend object (quadtree-node-objects node))
    (node-split node table))
  node)

(defun node-insert-extend (node object table)
  (declare (optimize speed))
  (if (region-contains-object-p node object)
      (node-insert node object table)
      (node-insert-extend (node-extend node (node-direction node object)) object table)))

(defun node-clear (node)
  (declare (optimize speed))
  (when (quadtree-node-active-p node)
    (prog1 (nconc (node-pop-objects node)
                  (loop for child in (node-children node)
                        nconcing (node-clear child)))
      (setf (quadtree-node-active-p node) NIL))))

(defun node-reorder (node table)
  (declare (optimize speed))
  (loop for object in (node-clear node)
        do (node-insert node object table)))

(defun node-check-activity (node)
  (declare (optimize speed))
  (when (and (node-empty-p node) (not (node-active-children-p node)))
    (setf (quadtree-node-active-p node) NIL)
    (when (quadtree-node-parent node)
      (node-check-activity (quadtree-node-parent node)))))

(defun node-remove (node object table)
  (declare (optimize speed))
  (multiple-value-bind (found others) ;; Clear the wanted object out.
      (loop until (node-empty-p node)
            for obj = (vector-pop (quadtree-node-objects node))
            for found = (when (eq obj object) obj)
            until found
            collect obj into rest
            finally (return (values found rest)))
    (loop for other in others ;; Put the others back.
          do (vector-push-extend other (quadtree-node-objects node)))
    (when (node-active-children-p node)
      (node-reorder node table))
    (node-check-activity node)
    found))

(defun node-find-all (node)
  (declare (optimize speed))
  (when (quadtree-node-active-p node)
    (nconc (node-list-objects node)
           (loop for child in (node-children node)
                 nconcing (node-find-all child)))))

(defun node-find (node region)
  (declare (optimize speed))
  (when (and (quadtree-node-active-p node) (region-overlaps-p node region))
    (if (region-contains-p region node)
        (node-find-all node)
        (nconc (node-list-objects node)
               (loop for child in (node-children node)
                     nconcing (node-find child region))))))

(defun node-set-min-size (node min-size)
  (declare (optimize speed))
  (setf (quadtree-node-min-size node) min-size)
  (loop for child in (node-children node)
        do (node-set-min-size child min-size)))

(defun node-set-threshold (node threshold)
  (declare (optimize speed))
  (setf (quadtree-node-threshold node) threshold)
  (loop for child in (node-children node)
        do (node-set-threshold child threshold)))

(defstruct (quadtree
            (:constructor make-quadtree ())
            (:copier NIL)
            (:predicate NIL))
  (root (%make-quadtree-node 0f0 0f0 100f0 100f0 0 1 1 NIL NIL NIL NIL NIL) :type quadtree-node)
  (table (make-hash-table :test 'eq) :type hash-table))

(defun make-quadtree-at (location size &key min-size threshold)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (declare (type (or null (unsigned-byte 16)) min-size))
  (let* ((tree (make-quadtree))
         (root (quadtree-root tree))
         (lx (vx2 location))
         (ly (vx2 location))
         (sx (vx2 size))
         (sy (vx2 size)))
    (when (node-children root) ;; Should not happen.
      (node-remove-children root :recurse T))
    (setf (vx4 root) lx)
    (setf (vy4 root) ly)
    (setf (vz4 root) (+ lx sx))
    (setf (vw4 root) (+ ly sy))
    (when (and min-size (< 0 min-size))
      (node-set-min-size root min-size))
    (when (and threshold (< 0 threshold))
      (node-set-threshold root threshold))
    tree))

(defun quadtree-insert (tree object)
  (declare (optimize speed))
  (when (gethash object (quadtree-table tree))
    (quadtree-remove tree object))
  (setf (quadtree-root tree) (node-insert-extend (quadtree-root tree) object (quadtree-table tree)))
  object)

(defun quadtree-remove (tree object)
  (declare (optimize speed))
  (let* ((table (quadtree-table tree))
         (node (gethash object table)))
    (when node
      (remhash object table)
      (node-remove node object table))))

(defun quadtree-update (tree object)
  (declare (optimize speed))
  (let ((node (gethash object (quadtree-table tree))))
    (when node
      (cond
        ((region-contains-object-p node object)
         ;; Might need a split if it can now be stored in a sub-node.
         (node-split node (quadtree-table tree)))
        (T ;; If it no longer fits, just reinsert.
         (quadtree-insert tree object)))
      object)))

(defun quadtree-find-all (tree)
  (declare (optimize speed))
  (node-find-all (quadtree-root tree)))

(defun quadtree-find-in-region (tree region)
  (declare (optimize speed))
  (declare (type vec4 region))
  (node-find (quadtree-root tree) region))

(defun quadtree-find-in (tree location size)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (let ((lx (vx2 location))
        (ly (vy2 location))
        (sx (vx2 size))
        (sy (vy2 size)))
    (quadtree-find-in-region tree (vec4 lx ly (+ lx sx) (+ ly sy)))))

(defun quadtree-find-for (tree object)
  (declare (optimize speed))
  (quadtree-find-in tree (location object) (bsize object)))

(defmethod trial:enter (object (tree quadtree))
  (quadtree-insert tree object))

(defmethod trial:leave (object (tree quadtree))
  (quadtree-remove tree object))

(defmethod trial::clear ((tree quadtree))
  (clrhash (quadtree-table tree))
  (node-clear (quadtree-root tree))
  tree)

(defun quadtree-print (tree)
  (format T "~&-------------------------")
  (labels ((recurse (node)
             (when (quadtree-node-active-p node)
               (format T "~&~v@{|  ~}└ ~a" (quadtree-node-depth node) node)
               (loop for child in (node-children node)
                     do (recurse child)))))
    (recurse (quadtree-root tree))))

(defun quadtree-lines (tree)
  (let ((points ()))
    (labels ((depth-color (depth)
               (let ((value (max 0.0 (- 1.0 (/ depth 100)))))
                 (vec 1 value value 0.1)))
             (recurse (node)
               (when (quadtree-node-active-p node)
                 (let ((color (depth-color (quadtree-node-depth node))))
                   (push (list (vxy_ node) color) points)
                   (push (list (vzy_ node) color) points)
                   (push (list (vxw_ node) color) points)
                   (push (list (vzw_ node) color) points)
                   (push (list (vxy_ node) color) points)
                   (push (list (vxw_ node) color) points)
                   (push (list (vzy_ node) color) points)
                   (push (list (vzw_ node) color) points)
                   (loop for child in (node-children node)
                         do (recurse child))))))
      (recurse (quadtree-root tree))
      points)))

(defun quadtree-check (tree) ;; None of these things should happen.
  (declare (optimize speed))
  (labels ((recurse (node)
             (when (and (not (quadtree-node-active-p node)) (node-active-children-p node))
               (error "Node ~a~%has active children without being active itself." node))
             (when (and (not (quadtree-node-active-p node))
                        (< 0 (length (quadtree-node-objects node))))
               (error "Node ~a~%has objects without being active itself." node))
             (when (and (quadtree-node-active-p node) (not (node-active-children-p node))
                        (= 0 (length (quadtree-node-objects node))))
               (error "Node ~a~%is active without active children or objects." node))
             (loop for child in (node-children node)
                   do (recurse child))
             (loop for object in (node-list-objects node)
                   unless (eq node (gethash object (quadtree-table tree)))
                   do (error "Node ~a~%is not assigned to object~%  ~a~% as it is assigned to~%node ~a"
                             node object (gethash object (quadtree-table tree))))))
    (recurse (quadtree-root tree)))
  (loop for object being the hash-keys of (quadtree-table tree)
        for node being the hash-values of (quadtree-table tree)
        do (loop for obj in (node-list-objects node)
                 for match = (eq obj object)
                 until match
                 finally (unless match
                           (error "Node ~a~%does not refer to object~%  ~a" node object)))))

(defun quadtree-reinsert-all (tree) ;; Useful only if something's gone very wrong.
  (let ((objects (node-clear (quadtree-root tree))))
    (clrhash (quadtree-table tree))
    (loop for object in objects do (quadtree-insert tree object))))

(defun call-all (function tree)
  (declare (optimize speed (safety 1)))
  (let ((function (etypecase function
                    (symbol (fdefinition function))
                    (function function))))
    (loop for object in (quadtree-find-all tree)
          do (funcall function object))))

(defun call-with-region (function tree region)
  (declare (optimize speed (safety 1)))
  (let ((function (etypecase function
                    (symbol (fdefinition function))
                    (function function))))
    (loop for object in (quadtree-find-in-region tree region)
          when (region-overlaps-p region object)
          do (funcall function object))))

(defun call-with-area (function tree location size)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (call-with-region function tree (vec4 (vx2 location) (vy2 location)
                                        (vx2 size) (vy2 size))))

(defun call-with-object (function tree object)
  (declare (optimize speed))
  (call-with-area function tree (location object) (bsize object)))
