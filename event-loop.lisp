#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *loop*)

(defgeneric add-handler (handler handler-container))
(defgeneric remove-handler (handler handler-container))
(defgeneric handle (event handler))

(defclass handler-container ()
  ((handlers :initform () :accessor handlers)))

(defmethod add-handler (handler (container handler-container))
  (setf (handlers container)
        (sort (cons handler (delete handler (handlers container) :test #'matches))
              #'> :key #'priority)))

(defmethod add-handler ((handlers list) (container handler-container))
  (let ((handlers (copy-list handlers)))
    (loop for cons on (handlers container)
          for handler = (find (car cons) handlers :test #'matches)
          do (when handler
               (setf (car cons) handler)
               (setf handlers (delete handler handlers))))
    (setf (handlers container) (sort (nconc handlers (handlers container)) #'> :key #'priority))))

(defmethod add-handler ((source handler-container) (container handler-container))
  (add-handler (handlers source) container))

(defmethod remove-handler (handler (container handler-container))
  (setf (handlers container)
        (delete handler (handlers container) :test #'matches)))

(defmethod remove-handler ((handlers list) (container handler-container))
  (setf (handlers container) (delete-if (lambda (el)
                                          (find el handlers :test #'matches))
                                        (handlers container))))

(defmethod remove-handler ((source handler-container) (container handler-container))
  (remove-handler (handlers source) container))

(defclass event-loop (handler-container)
  ((queue :initform (make-array 0 :initial-element NIL :adjustable T :fill-pointer T) :reader queue)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-instance event-type args)))))
    (vector-push-extend event (queue loop))))

(defun process (loop)
  (loop for i from 0
        while (< i (length (queue loop)))
        do (handle (aref (queue loop) i) loop)
           (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0))

(defmethod handle (event (loop event-loop))
  (let ((*loop* loop))
    (dolist (handler (handlers loop))
      (handle event handler))))

(defclass handler (named-entity)
  ((event-type :initarg :event-type :accessor event-type)
   (container :initarg :container :accessor container)
   (delivery-function :initarg :delivery-function :accessor delivery-function)
   (priority :initarg :priority :accessor priority))
  (:default-initargs
   :event-type (error "EVENT-TYPE required.")
   :container (error "CONTAINER required.")
   :delivery-function (error "DELIVERY-FUNCTION needed.")
   :priority 0))

(defmethod matches ((a handler) (b handler))
  (and (eq (container a) (container b))
       (eql (name a) (name b))))

(defmethod handle (event (handler handler))
  (when (typep event (event-type handler))
    (funcall (delivery-function handler) (container handler) event)))

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

(defclass subject-class (qtools:finalizable-class handler-container)
  ((effective-handlers :initform NIL :accessor effective-handlers)
   (instances :initform () :accessor instances)))

(defun cascade-option-changes (class)
  ;; Recompute effective handlers
  (loop with effective-handlers = (handlers class)
        for super in (c2mop:class-direct-superclasses class)
        when (c2mop:subclassp super 'subject-class)
        do (dolist (handler (effective-handlers super))
             (pushnew handler effective-handlers :test #'matches))
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
        when (and (c2mop:subclassp sub-class 'subject-class)
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
  (loop for handler in (handlers (class-of subject))
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

(defmethod add-handler (handler (class symbol))
  (add-handler handler (find-class class)))

(defmethod remove-handler (handler (class symbol))
  (remove-handler handler (find-class class)))

(defmethod enter :after ((subject subject) (loop event-loop))
  (push loop (loops subject)))

(defmethod leave :after ((subject subject) (loop event-loop))
  (setf (loops subject) (delete loop (loops subject))))

(defmacro define-subject (name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp c 'subject)) direct-superclasses)
    (push 'subject direct-superclasses))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass subject-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defclass event ()
  ())

(defclass tick (event)
  ())
