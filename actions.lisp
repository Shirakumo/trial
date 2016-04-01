#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *mappings* (make-hash-table :test 'equal))

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

(defun remove-action-mappings (action)
  (loop for k being the hash-keys of *mappings*
        do (when (and (consp k) (eql (car k) action))
             (remhash k *mappings*))))

(defmacro define-action (name &body mappings)
  (destructuring-bind (name &key (superclasses '(action))) (enlist name)
    (flet ((compile-mapping (mapping)
             (destructuring-bind (type &rest tests) mapping
               `(define-simple-mapping (,name ,type) (,type ,name)
                  ,@tests))))
      `(progn
         (defclass ,name ,superclasses
           ())
         (remove-action-mappings ',name)
         ,@(mapcar #'compile-mapping mappings)))))

(define-action move-left
  (key-press (eql key :a))
  (gamepad-move (eql axis :left-h) (< new-pos -0.2))
  (gamepad-press (eql button :dpad-left)))

(define-action move-right
  (key-press (eql key :d))
  (gamepad-move (eql axis :left-h) (< 0.2 new-pos))
  (gamepad-press (eql button :dpad-right)))

(define-action move-up
  (key-press (eql key :w))
  (gamepad-move (eql axis :left-v) (< 0.2 new-pos))
  (gamepad-press (eql button :dpad-up)))

(define-action move-down
  (key-press (eql key :s))
  (gamepad-move (eql axis :left-v) (< new-pos -0.2))
  (gamepad-press (eql button :dpad-down)))

(defmethod handle :before (event (loop event-loop))
  (map-event event loop))
