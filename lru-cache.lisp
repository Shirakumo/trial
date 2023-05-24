#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defstruct (lru-cache-node
            (:constructor make-lru-cache-node (left right))
            (:predicate NIL)
            (:copier NIL))
  (left NIL :type T)
  (right NIL :type T)
  (value NIL :type T))

(defmethod print-object ((node lru-cache-node) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (format stream "~a" (lru-cache-node-value node))))

(defstruct (lru-cache
            (:constructor %make-lru-cache (head table size))
            (:predicate NIL)
            (:copier NIL))
  (head NIL :type lru-cache-node)
  (table NIL :type hash-table)
  (size 0 :type (unsigned-byte 32)))

(defmethod print-object ((cache lru-cache) stream)
  (print-unreadable-object (cache stream :type T :identity T)
    (format stream "~a" (lru-cache-size cache))))

(defmethod describe-object ((cache lru-cache) stream)
  (format stream "~a~%  [~s]~%~%" cache (type-of cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for i from 0
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        for value = (lru-cache-node-value node)
        until (eq node tail)
        do (when value
             (format stream "~3d ~a~%" i value))))

(defun make-lru-cache (size)
  (check-type size (integer 1))
  (let ((head (make-lru-cache-node NIL NIL)))
    (setf (lru-cache-node-left head) head)
    (setf (lru-cache-node-right head) head)
    (loop repeat (1- size)
          for node = (make-lru-cache-node head (lru-cache-node-right head))
          do (setf (lru-cache-node-left (lru-cache-node-right head)) node)
             (setf (lru-cache-node-right head) node))
    (%make-lru-cache head (make-hash-table :test 'eq :size size) size)))

;; Returns T if the value did not exist in the cache.
(defun lru-cache-push (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let* ((table (lru-cache-table cache))
         (head (lru-cache-head cache))
         (node (gethash value table)))
    (cond ((null node)
           ;; Claim the oldest (left of the head) node and shift up
           (let ((prev (the lru-cache-node (lru-cache-node-left head))))
             (setf (lru-cache-node-value prev) value)
             (setf (lru-cache-head cache) prev)
             (when (lru-cache-node-value prev)
               (remhash (lru-cache-node-value prev) table))
             (setf (gethash value table) prev))
           T)
          ((eq node head)
           NIL)
          (T
           (let ((l (the lru-cache-node (lru-cache-node-left node)))
                 (r (the lru-cache-node (lru-cache-node-right node))))
             ;; Fuse neighbours to remove the node
             (setf (lru-cache-node-right l) r)
             (setf (lru-cache-node-left r) l)
             ;; Inject the node as the new head
             (setf (lru-cache-head cache) node)
             (setf (lru-cache-node-left node) (lru-cache-node-left head))
             (setf (lru-cache-node-right node) head)
             (setf (lru-cache-node-left head) node)
             NIL)))))

;; Returns T if the value existed in the cache.
(defun lru-cache-pop (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let* ((table (lru-cache-table cache))
         (head (lru-cache-head cache))
         (node (gethash value table)))
    (cond ((null node)
           NIL)
          ((eq node head)
           (setf (lru-cache-node-value node) NIL)
           (remhash value table)
           (setf (lru-cache-head cache) (lru-cache-node-right node))
           T)
          (T
           (let ((l (the lru-cache-node (lru-cache-node-left node)))
                 (r (the lru-cache-node (lru-cache-node-right node))))
             ;; Fuse neighbours to remove the node
             (setf (lru-cache-node-right l) r)
             (setf (lru-cache-node-left r) l)
             ;; Inject the node as the new tail
             (setf (lru-cache-node-right node) (lru-cache-node-left head))
             (setf (lru-cache-node-right node) head)
             (setf (lru-cache-node-left head) node)
             T)))))

(defun map-lru-cache (function cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        with function = (etypecase function
                          (symbol (fdefinition function))
                          (function function))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        for value = (lru-cache-node-value node)
        until (eq tail node)
        do (if value
               (funcall function value)
               (return)))
  cache)

(defmacro do-lru-cache ((element cache &optional result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,element)
                ,@body))
         (declare (dynamic-extent #',thunk))
         (map-lru-cache #',thunk ,cache)
         ,result))))
