#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass subject-class-redefined (event)
  ((subject-class :initarg :subject-class :reader subject-class)))

(defclass subject-class (qtools:finalizable-class handler-container)
  ((effective-handlers :initform NIL :accessor effective-handlers)
   (instances :initform () :accessor instances)))

(defmethod cascade-option-changes ((class subject-class))
  ;; Recompute effective handlers
  (loop with effective-handlers = (handlers class)
        for super in (c2mop:class-direct-superclasses class)
        do (when (typep super 'subject-class)
             (dolist (handler (effective-handlers super))
               (pushnew handler effective-handlers :key #'name)))
        finally (setf (effective-handlers class) effective-handlers))
  ;; Update instances
  (loop for pointer in (instances class)
        for subject = (tg:weak-pointer-value pointer)
        when subject
        collect (prog1 pointer
                  (reinitialize-instance subject)) into instances
        finally (setf (instances class) instances))
  ;; Notify
  (loop for pointer in (instances class)
        for subject = (tg:weak-pointer-value pointer)
        when (and subject (slot-value subject 'event-loop))
        return (issue (slot-value subject 'event-loop) 'subject-class-redefined
                      :subject-class class)))

(defmethod cascade-option-changes :after ((class subject-class))
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
  ((event-loop :initarg :event-loop :accessor event-loop))
  (:default-initargs
   :event-loop NIL)
  (:metaclass subject-class))

(defmethod initialize-instance :after ((subject subject) &key)
  (push (tg:make-weak-pointer subject) (instances (class-of subject)))
  (regenerate-handlers subject))

(defmethod reinitialize-instance :after ((subject subject) &key)
  (regenerate-handlers subject))

(defmethod regenerate-handlers ((subject subject))
  ;; During recompilation the EVENT-LOOP method might
  ;; be temporarily unavailable
  (let ((loop (slot-value subject 'event-loop)))
    (when loop
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
    (when loop
      (add-handler subject loop))))

(defmethod register :before ((subject subject) (loop event-loop))
  (when (event-loop subject)
    (error "~s is already registered on the event-loop ~s, can't add it to ~s."
           subject (event-loop subject) loop)))

(defmethod register :after ((subject subject) (loop event-loop))
  (setf (event-loop subject) loop))

(defmethod deregister :before ((subject subject) (loop event-loop))
  (unless (eql loop (event-loop subject))
    (error "~s is registered on the event-loop ~s, can't remove it from ~s."
           subject (event-loop subject) loop)))

(defmethod deregister :after ((subject subject) (loop event-loop))
  (setf (event-loop subject) NIL))

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
