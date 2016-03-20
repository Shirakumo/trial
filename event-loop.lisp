#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *event-loop* NIL)

(defclass synchronous-event-loop (deeds:event-loop)
  ())

(defmethod deeds:issue ((event deeds:event) (loop synchronous-event-loop))
  (deeds:handle event loop))

(defmacro define-handler ((name event-type) args &body body)
  `(deeds:define-handler (,name ,event-type) ,args
     :class 'locally-blocking-handler
     :loop *event-loop*
     ,@body))

(defun issue (event-type &rest args)
  (deeds:issue (apply #'make-instance event-type args) *event-loop*))
