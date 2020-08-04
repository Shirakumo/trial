#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +retention-table* (make-hash-table :test 'eql))
(defvar *mappings* (make-hash-table :test 'equal))

(declaim (inline retained (setf retained) clear-retained))
(defun retained (id)
  (gethash id +retention-table*))

(defun (setf retained) (bool id)
  (setf (gethash id +retention-table*) bool))

(defun clear-retained ()
  (clrhash +retention-table*))

(defun mapping (name)
  (gethash name *mappings*))

(defun (setf mapping) (mapping name)
  (setf (gethash name *mappings*) mapping))

(defun remove-mapping (name)
  (remhash name *mappings*))

(defmacro define-mapping (name (loop ev) &body body)
  `(setf (mapping ',name)
         (lambda (,loop ,ev)
           ,@body)))

(defmacro define-simple-mapping (name (from to &rest to-args) &body tests)
  (let ((loop (gensym "LOOP")))
    `(define-mapping ,name (,loop ,from)
       (when (typep ,from ',from)
         (with-all-slots-bound (,from ,from)
           (when (and ,@tests)
             (issue ,loop (make-instance ',to ,@to-args))))))))

(defun map-event (event loop)
  (loop for function being the hash-values of *mappings*
        do (funcall function loop event)
        do (when result (issue loop result))))

(defclass action (event)
  ((source-event :initarg :source-event :initform NIL :accessor source-event)))

(defun remove-action-mappings (action)
  (loop for k being the hash-keys of *mappings*
        do (when (and (consp k) (eql (car k) action))
             (remhash k *mappings*))))

(defmacro define-action (name superclasses &body mappings)
  (flet ((compile-mapping (mapping)
           (destructuring-bind (type &rest tests) mapping
             `(define-simple-mapping (,name ,type) (,type ,name :source-event ,type)
                ,@tests))))
    `(progn
       (defclass ,name ,(or superclasses '(action))
         ())
       (remove-action-mappings ',name)
       ,@(mapcar #'compile-mapping mappings))))
