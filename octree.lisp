#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial)

(defparameter *max-octree-life-span* 64)

(define-subject octree (bound-entity pivoted-entity)
  ((parent :initarg :parent :accessor parent)
   (treshold :initarg :treshold :accessor treshold)
   (active-children :initform 0 :accessor active-children) ;; for later optimization
   (children :initform (make-array 8 :initial-element NIL) :accessor children)
   (built-p :initform NIL :accessor built-p)
   (life :initform -1 :accessor life)
   (life-span :initform 8 :accessor life-span)
   (objects :initform NIL :accessor objects)
   (pending-objects :initform NIL :accessor pending-objects)
   (ready-p :initform NIL :accessor ready-p))
  (:default-initargs
   :parent NIL
   :treshold 4))

(defmethod initialize-instance :after ((octree octree) &key)
  (when (v= 0 (pivot octree))
    (let ((size (v/ (bounds octree) 2)))
      (setf (pivot octree) (v- 0 (vec (vx size) 0 (vz size)))))))

(defmethod enter ((object collidable-entity) (octree octree))
  (when octree (push object (pending-objects octree)))
  object)

(defmethod leave ((object collidable-entity) (octree octree))
  ;; FIXME
  )

(defmethod build-tree ((octree octree))
  "Builds children for the given octree if and only if it needs to split."
  (when (and (< (treshold octree) (length (objects octree)))
             (v< 1 (bounds octree)))
    (let* ((octant (sub-octant octree))
           (child-objects (make-array (array-dimension octant 0)
                                      :initial-element NIL))
           (cur-objects (objects octree))
           (new-objects NIL))
      ;; Find who belongs where
      (loop while cur-objects
            do (let ((object (pop cur-objects))
                     (set-p NIL))
                 (when (v< 0 (bounds object))
                   (for:for ((i repeat (array-dimension octant 0))
                             (sub-box across octant))
                     (when (contains sub-box object)
                       (push object (elt child-objects i))
                       (setf set-p T)
                       (return))))
                 (unless set-p
                   (push object new-objects))))
      (setf (objects octree) new-objects)
      
      ;; Create the child nodes
      (for:for ((i repeat (array-dimension octant 0))
                (sub-box across octant)
                (children across child-objects))
        (when children
          (let ((child (make-instance 'octree
                                      :bounds (bounds sub-box)
                                      :location (location sub-box)
                                      :parent octree
                                      :treshod (treshold octree))))
            (for:for ((object in children))
              (enter object child))
            (build-tree child)
            (setf (elt (children octree) i) child)
            (setf (active-children octree)
                  (logior (active-children octree)
                          (ash 1 i))))))
      (setf (built-p octree) T
            (ready-p octree) T))))

(defmethod insert-object ((octree octree) (object collidable-entity))
  "Inserts an object into the octree."
  (cond ((or (and (<= (length (objects octree)) (treshold octree))
                  (/= 0 (active-children octree)))
             (v<= (bounds object) 1))
         ;; Equal bounds and there's space, make it part of this area
         ;; TODO: If there are equal bounds but no room, should we spread
         ;;       the existing children into sub-trees and keep this one here?
         (push object (objects octree)))
        ((contains octree object)
         (let ((octant (sub-octant octree))
               (found NIL))
           ;; Let's find a child where this fits
           (for:for ((i repeat (array-dimension octant 0))
                     (sub-box across octant)
                     (child across (children octree)))
             (when (contains sub-box object)
               (if child
                   (insert-object child object)
                   (let ((child (make-instance 'octree
                                               :bounds (bounds sub-box)
                                               :location (location sub-box)
                                               :parent octree
                                               :treshold (treshold octree))))
                     (insert-object child object)
                     (setf (elt (children octree) i) child)
                     (setf (active-children octree) ;; TODO: For future optimisations
                           (logior (active-children octree)
                                   (ash 1 i)))))
               (setf found T)
               (return)))
           (unless found
             (push object (objects octree)))))
        (T ;; It's out of bounds or intersects. Just ignore.
         (build-tree octree)))) ;; Rebuild just in case.

(defmethod intersections ((octree octree) &optional parent-objects)
  "Returns intersections that happen with the objects in the octree."
  (let ((isects NIL)
        (local-objs (copy-list (objects octree))))
    (for:for ((parent-obj in parent-objects))
      (for:for ((obj in local-objs))
        (let ((isection (intersects parent-obj obj)))
          (when isection (push isection isects)))))
    (when local-objs
      (let ((tmp-objects (copy-list local-objs)))
        (for:for ()
          (while tmp-objects)
          (let ((cur-obj (pop tmp-objects)))
            (for:for ((other-obj in tmp-objects))
              (let ((isect (intersects cur-obj other-obj)))
                (when isect (push isect isects))))))))
    ;; Merge and pass them on
    (when (/= 0 (active-children octree))
      (nconc local-objs parent-objects)
      (for:for ((child across (children octree)))
        (when child
          (nconc isects (intersections child local-objs)))))
    isects))

(defmethod sub-octant ((octree octree))
  (let* ((half (v/ (bounds octree) 2))
         (quarter (v/ half 2))
         (three-quarter (v+ half quarter))
         (octant (make-array 8 :fill-pointer 0)))
    ;; All the regions for each sub-octant
    (vector-push (make-instance 'bound-entity :location quarter :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx three-quarter) (vy quarter) (vz quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx quarter) (vy three-quarter) (vz quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx quarter) (vy quarter) (vz three-quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx three-quarter) (vy three-quarter) (vz quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx three-quarter) (vy quarter) (vz three-quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx three-quarter) (vy three-quarter) (vz three-quarter))
                                :bounds half) octant)
    (vector-push (make-instance 'bound-entity
                                :location (vec (vx quarter) (vy three-quarter) (vz three-quarter))
                                :bounds half) octant)
    octant))

(defmethod update-tree ((octree octree))
  "Inserts the pending objects into the three"
  (let ((pending (pending-objects octree)))
    (cond ((built-p octree)
           (for:for ()
             (while pending)
             (insert-object octree (pop pending)))
           (setf (pending-objects octree) NIL))
          (T
           (for:for ()
             (while pending)
             (push (pop pending) (objects octree)))
           (setf (pending-objects octree) NIL)
           (build-tree octree))))
  (setf (ready-p octree) T))

(defmethod update-cycle ((octree octree))
  (when (built-p octree)
    ;; Set the life span of this node
    (if (objects octree)
        (when (= 0 (active-children octree))
          (if (< (life octree) 0)
              (setf (life octree) (1- (life-span octree))) ;; start death count
              (decf (life octree)))) ;; decrease life
        (when (<= 0 (life octree))
          ;; objects added while dying, increase lifespan and quit death count
          (when (< (life-span octree) *max-octree-life-span*)
            ;; Give it a bigger life span as it seems this octree is used more often
            (incf (life-span octree) (life-span octree)))
          (setf (life octree) -1)))

    ;; Get the moved objects
    (let ((new-objects NIL)
          (moved-objects NIL))
      (for:for ()
        (while (objects octree))
        (let ((object (pop (objects octree))))
          (if (contains octree object)
              (push object new-objects)
              (push object moved-objects))))
      (setf (objects octree) new-objects)

      ;; Update children here so they might give us their moved objects
      (for:for ((child across (children octree)))
        (when child (update-cycle child)))

      ;; Move the moved objects
      (let ((current (parent octree)))
        (for:for ()
          (while (and current moved-objects))
          (let ((object (pop moved-objects)))
            (when (contains current object)
              (insert-object current (pop moved-objects)))))
        (setf current (parent current))))

    ;; Prune dead children
    (for:for ((i repeat (array-dimension (children octree) 0))
              (child across (children octree)))
      (when (= 0 (active-children child) (life child))
        (setf (elt (children octree) i) NIL
              (active-children octree) (logxor (active-children octree) (ash 1 i)))))

    ;; Finally moved everything right, let's check those collisions
    (unless (parent octree)
      (loop for intersection in (intersections octree)
            do (handle-collision (first-object intersection) intersection)
               (handle-collision (second-object intersection) intersection)))))

(define-handler (octree tick) (ev)
  (update-cycle octree))

(define-handler (octree enter) (ev entity)
  (when (typep entity 'collidable-entity)
    (enter entity octree)))

(define-handler (octree leave) (ev entity)
  (when (typep entity 'collidable-entity)
    (leave entity octree)))
