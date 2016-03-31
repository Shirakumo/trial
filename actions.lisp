#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *mappings* (make-hash-table :test 'eql))

(defun mapping (name)
  (gethash name *mappings*))

(defun (setf mapping) (mapping name)
  (setf (gethash name *mappings*) mapping))

(defun remove-mapping (name)
  (remhash name *mappings*))

(defmacro define-mapping ((name event-type) args &body body)
  (let ((ev (or (first args) (gensym "EVENT"))))
    `(setf (mapping ',name)
           (lambda (,ev)
             (when (typep ,ev ',event-type)
               (with-slots ,(rest args) ,ev
                 ,@body))))))

(defmacro define-simple-mapping (name (from to) &body tests)
  (let ((ev (gensym "EVENT"))
        (to (enlist to)))
    `(setf (mapping ',name)
           (lambda (,ev)
             (when (typep ,ev ',from)
               (with-all-slots-bound (,ev ,from)
                 (when (and ,@tests)
                   (make-instance ',(first to) ,@(rest to)))))))))

;; Currently this system is very unoptimised as it has to
;; loop through all potential mappers every time. Optimisation
;; could be done by separating out the event-type test somehow.
(defun map-event (event loop)
  (loop for function being the hash-values of *mappings*
        for result = (funcall function event)
        do (when result (issue loop result))))

(defclass action (event)
  ())

(defmacro define-action (name &body mappings)
  (destructuring-bind (name &key (superclasses '(action))) (enlist name)
    (flet ((compile-mapping (mapping)
             (destructuring-bind (type &rest tests) mapping
               (let ((mapping (intern (format NIL "~a-~a" name type))))
                 `(define-simple-mapping ',mapping (,type ,name)
                    ,@tests)))))
      `(progn
         (defclass ,name ,superclasses
           ())
         ,@(mapcar #'compile-mapping mappings)))))
