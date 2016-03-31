#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *mappings* (make-hash-table :test 'eql))

(defun mappings (event-type)
  (gethash event-type *mappings*))

(defun (setf mappings) (mappings event-type)
  (setf (gethash event-type *mappings*) mappings))

(defun remove-mappings (event-type)
  (remhash event-type *mappings*))

(defun add-mapping (event-type name function)
  (let ((cons (find name (mappings event-type) :key #'car)))
    (if cons
        (setf (cdr cons) function)
        (push (cons name function) (mappings event-type))))
  (list event-type name))

(defun remove-mapping (event-type name)
  (setf (mappings event-type) (remove name (mappings event-type) :key #'car)))

(defmacro define-mapping ((event-type name) args test &body body)
  (let ((ev (or (first args) (gensym "EVENT"))))
    `(add-mapping ',event-type ',name
                  (lambda (,ev)
                    (with-slots ,(rest args) ,ev
                      (when ,test
                        ,@body))))))

(defun map-event (event loop)
  (loop for (name . function) in (mappings (type-of event))
        for result = (funcall function event)
        when result (issue loop result)))

(defclass action (event)
  ())

(defmacro define-action (name &body mappings)
  (destructuring-bind (name &key (superclasses '(action))) (enlist name)
    (flet ((compile-mapping (mapping)
             (destructuring-bind (type &rest tests) mapping
               (let ((ev (gensym "EVENT")))
                 `(add-mapping ',type ',name
                               (lambda (,ev)
                                 (with-all-slots-bound (,ev ,type)
                                   (and ,@tests))))))))
      `(progn
         (defclass ,name ,superclasses
           ())
         (remove-mappings ',name)
         ,@(mapcar #'compile-mapping mappings)))))
