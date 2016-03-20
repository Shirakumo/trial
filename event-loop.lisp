#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric add-handler (handler handler-container))
(defgeneric remove-handler (handler handler-container))
(defgeneric handle (event handler))

(defclass handler-container ()
  ((handlers :initform () :accessor handlers)))

(defmethod add-handler (handler (container handler-container))
  (setf (handlers container)
        (cons handler (remove handler (handlers container) :test #'matches))))

(defmethod add-handler ((handlers list) (container handler-container))
  (loop for handler in (handlers container)
        unless (find handler handlers :test #'matches)
        do (push handler handlers))
  (setf (handlers container) handlers))

(defmethod remove-handler (handler (container handler-container))
  (setf (handlers container)
        (remove handler (handlers container) :test #'matches)))

(defmethod remove-handler ((handlers list) (container handler-container))
  (loop for handler in (handlers container)
        unless (find handler handlers :test #'matches)
        collect handler into cleaned-handlers
        finally (setf (handlers container) cleaned-handlers)))

(defclass event-loop (handler-container)
  ((queue :initform (make-array 0 :initial-element NIL :adjustable T :fill-pointer T) :reader queue)))

(defvar *event-loop* (make-instance 'event-loop))

(defun issue (event-type &rest args)
  (vector-push-extend (apply #'make-instance event-type args) (queue *event-loop*)))

(defun process (loop)
  (loop for i from 0
        while (< i (length (queue loop)))
        do (handle (aref (queue loop) i) loop)
           (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0))

(defmethod handle (event (loop event-loop))
  (dolist (handler (handlers loop))
    (handle handler event)))

(defclass handler (named-entity)
  ((event-type :initarg :event-type :accessor event-type)
   (container :initarg :container :accessor container)
   (delivery-function :initarg :delivery-function :accessor delivery-function))
  (:default-initargs
   :event-type (error "EVENT-TYPE required.")
   :container (error "CONTAINER required.")
   :delivery-function (error "DELIVERY-FUNCTION needed.")))

(defmethod handle (event (handler handler))
  (when (typep event (event-type handler))
    (funcall (delivery-function handler) (container handler) event)))

(defmacro define-handler ((class name) event-type args &body body)
  (let ((event (first args))
        (args (rest args)))
    `(add-handler (make-instance
                   'handler
                   :name ',name
                   :event-type ',event-type
                   :container ',class
                   :delivery-function (lambda (,class ,event)
                                        (with-slots ,args ,event
                                          ,@body)))
                  ',class)))

(defclass subject-class (standard-class handler-container)
  ((effective-handlers :initform NIL :accessor effective-handlers)))

(defmethod c2mop:validate-superclass ((class subject-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass subject-class))
  T)

(defmethod c2mop:validate-superclass ((class subject-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class subject-class) (superclass subject-class))
  T)

(defun cascade-option-changes (class)
  (loop with effective-handlers = ()
        for super in (c2mop:class-direct-superclasses class)
        when (c2mop:subclassp super 'subject-class)
        do (dolist (handler (effective-handlers super))
             (pushnew handler effective-handlers :test #'matches)))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class 'subject-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class subject-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))

(defmethod )

(defclass subject (handler-container)
  ()
  (:metaclass subject-class))

(defmethod add-handler (handler (class symbol))
  (add-handler handler (find-class class)))

(defmethod remove-handler (handler (class symbol))
  (remove-handler handler (find-class class)))

(defclass event ()
  ())

(defclass tick (event)
  ())
