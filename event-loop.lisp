#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass synchronous-event-loop (deeds:compiled-event-loop)
  ((queue :initform (make-array 0 :element-type 'deeds:event :adjustable T :fill-pointer T) :reader queue)))

(defvar *event-loop* (make-instance 'synchronous-event-loop))

(defmethod deeds:issue ((event deeds:event) (loop synchronous-event-loop))
  (vector-push-extend event (queue loop)))

(defmethod deeds:start ((loop synchronous-event-loop))
  (setf (fill-pointer (queue loop)) 0)
  loop)

(defmethod deeds:stop ((loop synchronous-event-loop))
  loop)

(defmacro define-handler ((name event-type) args &body body)
  `(deeds:define-handler (,name ,event-type) ,args
     :class 'deeds:locally-blocking-handler
     :loop *event-loop*
     ,@body))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  `(deeds:define-event ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defun issue (event-type &rest args)
  (deeds:issue (apply #'make-instance event-type args) *event-loop*))

(defun process (loop)
  (loop for i from 0
        while (< i (length (queue loop)))
        do (deeds:handle (aref (queue loop) i) loop)
           (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0))

(define-event trial-event ()
  ())

(define-event tick (trial-event)
  ())
