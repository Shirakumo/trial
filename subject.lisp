#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass subject-class (qtools:finalizable-class handler-container)
  ((effective-handlers :initform NIL :accessor effective-handlers)
   (instances :initform () :accessor instances)))

(defun cascade-option-changes (class)
  ;; Recompute effective handlers
  (loop with effective-handlers = (handlers class)
        for super in (c2mop:class-direct-superclasses class)
        do (when (typep super 'subject-class)
             (dolist (handler (effective-handlers super))
               (pushnew handler effective-handlers :key #'name)))
        finally (setf (effective-handlers class) effective-handlers))
  ;; Update instances
  (loop for pointer in (instances class)
        for value = (tg:weak-pointer-value pointer)
        when value
        collect (prog1 pointer
                  (reinitialize-instance value)) into instances
        finally (setf (instances class) instances))
  ;; Propagate
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (typep sub-class 'subject-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class subject-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))

(defmethod add-handler :after (handler (class subject-class))
  (cascade-option-changes class))

(defmethod remove-handler :after (handler (class subject-class))
  (cascade-option-changes class))

(defmethod add-handler (handler (class symbol))
  (add-handler handler (find-class class)))

(defmethod remove-handler (handler (class symbol))
  (remove-handler handler (find-class class)))

(defclass subject (entity finalizable handler-container)
  ((loops :initarg :loops :accessor loops))
  (:default-initargs
   :loops ())
  (:metaclass subject-class))

(defmethod initialize-instance :after ((subject subject) &key)
  (push (tg:make-weak-pointer subject) (instances (class-of subject)))
  (regenerate-handlers subject))

(defmethod reinitialize-instance :after ((subject subject) &key)
  (regenerate-handlers subject))

(defmethod regenerate-handlers ((subject subject))
  (dolist (loop (loops subject))
    (remove-handler subject loop))
  (loop for handler in (effective-handlers (class-of subject))
        collect (make-instance
                 'handler
                 :container subject
                 :name (name handler)
                 :event-type (event-type handler)
                 :priority (priority handler)
                 :delivery-function (delivery-function handler)) into handlers
        finally (setf (handlers subject) handlers))
  (dolist (loop (loops subject))
    (add-handler subject loop)))

(defmethod register :after ((subject subject) (loop event-loop))
  (push loop (loops subject)))

(defmethod deregister :after ((subject subject) (loop event-loop))
  (setf (loops subject) (delete loop (loops subject))))

(defmacro define-subject (&environment env name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp (find-class c T env) 'subject)) direct-superclasses)
    (setf direct-superclasses (append direct-superclasses (list 'subject))))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass subject-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defmacro define-handler ((class name &optional (event-type name) (priority 0)) args &body body)
  (let ((event (first args))
        (args (rest args)))
    `(add-handler (make-instance
                   'handler
                   :name ',name
                   :event-type ',event-type
                   :container ',class
                   :priority ,priority
                   :delivery-function (lambda (,class ,event)
                                        (declare (ignorable ,class ,event))
                                        (with-slots ,args ,event
                                          ,@body)))
                  ',class)))

(defmacro define-generic-handler ((class name &optional (event-type name) (priority 0)) &body options)
  `(progn
     (defgeneric ,name (,class event)
       ,@options)
     (add-handler (make-instance
                   'handler
                   :name ',name
                   :event-type ',event-type
                   :container ',class
                   :priority ,priority
                   :delivery-function #',name)
                  ',class)))
