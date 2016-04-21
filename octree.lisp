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
  (unless (null octree)
    (push object (pending-objects octree)))
  object)

(defmethod leave ((object collidable-entity) (octree octree))
  ;; FIXME
  )

(defmethod build-tree ((octree octree))
  (when (and (< (treshold octree) (length (objects octree)))
             (v< 1 (bounds octree)))
    (let ((octant (sub-octant octree))
          (child-objects (make-array 8 :initial-element NIL))
          (cur-objects (objects octree))
          (new-objects NIL))
      ;; Find who belongs where
      (loop while cur-objects
            do (let ((object (pop cur-objects))
                     (set-p NIL))
                 (when (v< 0 (bounds object))
                   (dotimes (i 8)
                     (let ((sub-box (elt octant i)))
                       (when(contains sub-box object)
                         (push object (elt child-objects i))
                         (setf set-p T)
                         (return)))))
                 (unless set-p
                   (push object new-objects))))
      (setf (objects octree) new-objects)
      
      ;; Create the child nodes
      (dotimes (i 8)
        (let ((sub-box (elt octant i)))
          (when (elt child-objects i)
            (let ((child (make-instance 'octree
                                        :bounds (bounds sub-box)
                                        :location (location sub-box)
                                        :parent octree
                                        :treshold (treshold octree))))
              (loop for obj in (elt child-objects i)
                    do (enter obj child))
              (build-tree child)
              (setf (elt (children octree) i) child)
              (setf (active-children octree)
                    (logior (active-children octree)
                            (ash 1 i)))))))
      (setf (built-p octree) T
            (ready-p octree) T))))

(defmethod insert-object ((octree octree) (object collidable-entity))
  (cond ((or (and (<= (length (objects octree)) (treshold octree))
                  (/= 0 (active-children octree)))
             (v<= (bounds object) 1))
         (push object (objects octree)))
        ((contains octree object)
         (let ((octant (sub-octant octree))
               (found NIL))
           ;; Let's find a child where this fits
           (dotimes (i 8)
             (let ((sub-box (elt octant i))
                   (child (elt (children octree) i)))
               (when (contains sub-box object)
                 (cond (child
                        (insert-object child object))
                       (T
                        (let ((child (make-instance 'octree
                                                    :bounds (bounds sub-box)
                                                    :location (location sub-box)
                                                    :parent octree
                                                    :treshold (treshold octree))))
                          (insert-object child object)
                          (setf (elt (children octree) i) child)
                          (setf (active-children octree)
                                (logior (active-children octree)
                                        (ash 1 i))))))
                 (setf found T)
                 (return))))
           (unless found
             (push object (objects octree)))))
        (T ;; It's out of bounds or intersects. Just ignore? I don't know.
         (build-tree octree)))) ;; Rebuild just in case.

(defmethod intersections ((octree octree) &optional parent-objects)
  (let ((isects NIL) (local-objs (copy-list (objects octree))))
    (loop for parent-obj in parent-objects
          do (loop for obj in local-objs
                   do (let ((isection (intersects parent-obj obj)))
                        (when isection (push isection isects)))))
    (when (< 0 (length local-objs))
      (let ((tmp-objects (copy-list local-objs)))
        (loop while tmp-objects
              do (let ((cur-obj (pop tmp-objects)))
                   (loop for other-obj in tmp-objects
                         do (let ((isect (intersects cur-obj other-obj)))
                              (when isect (push isect isects))))))))
    ;; Merge and pass them on
    (when (/= 0 (active-children octree))
      (nconc local-objs parent-objects)
      (loop for child across (children octree)
            do (when child
                 (nconc isects (intersections child local-objs)))))))

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
  (let ((pending (pending-objects octree)))
    (cond ((built-p octree)
           (loop while pending
                 do (insert-object octree (pop pending)))
           (setf (pending-objects octree) NIL))
          (T
           (loop while pending
                 do (push (pop pending) (objects octree)))
           (setf (pending-objects octree) NIL)
           (build-tree octree))))
  (setf (ready-p octree) T))

(defmethod update-cycle ((octree octree))
  (when (built-p octree)
    ;; Set the life span of this node
    (if (< 0 (length (objects octree)))
        (when (= 0 (active-children octree))
          (if (< (life octree) 0)
              (setf (life octree) (life-span octree)) ;; start death count
              (decf (life octree)))) ;; decrease life
        (when (<= 0 (life octree))
          ;; objects added while dying, increase lifespan and quit death count
          (when (< (life-span octree) *max-octree-life-span*)
            (incf (life-span octree) (life-span octree)))
          (setf (life octree) -1)))

    ;; Get the moved objects
    (let ((new-objects NIL)
          (moved-objects NIL))
      (loop while (objects octree)
            do (let ((object (pop (objects octree))))
                 (when (alive-p object) ;; Prune the dead
                   (if (contains octree object)
                       (push object new-objects)
                       (push object moved-objects)))))
      (setf (objects octree) new-objects)

      ;; Update children here so they might give us their moved objects
      (loop for child across (children octree)
            do (when child (update-cycle child)))

      ;; Move the moved objects
      (let ((current (parent octree)))
        (loop while (and current moved-objects)
              do (let ((object (pop moved-objects)))
                   (when (contains current object)
                     (insert-object current (pop moved-objects)))))
        (setf current (parent current))))

    ;; Prune dead children
    (dotimes (i 8)
      (let ((child (elt (children octree) i)))
        (when (= 0 (active-children child) (life child))
          (setf (elt (children octree) i) NIL
                (active-children octree)
                (logxor (active-children octree) (ash 1 i))))))

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
