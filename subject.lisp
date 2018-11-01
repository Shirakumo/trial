#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass subject-class (standard-class handler-container)
  ((effective-handlers :initform NIL :accessor effective-handlers)))

(defmethod c2mop:validate-superclass ((class subject-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass subject-class))
  NIL)

(defmethod c2mop:validate-superclass ((class subject-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class subject-class) (superclass subject-class))
  T)

(defmethod compute-effective-handlers ((class subject-class))
  ;; Recompute effective handlers
  (loop with effective-handlers = (handlers class)
        for super in (c2mop:class-direct-superclasses class)
        do (when (typep super 'subject-class)
             (dolist (handler (effective-handlers super))
               (pushnew handler effective-handlers :key #'name)))
        finally (return effective-handlers)))

(defmethod c2mop:finalize-inheritance :after ((class subject-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (setf (effective-handlers class) (compute-effective-handlers class)))

(defmethod add-handler :after (handler (class subject-class))
  (when (c2mop:class-finalized-p class)
    (reinitialize-instance class)))

(defmethod remove-handler :after (handler (class subject-class))
  (when (c2mop:class-finalized-p class)
    (reinitialize-instance class)))

(defmethod add-handler (handler (class symbol))
  (add-handler handler (find-class class)))

(defmethod remove-handler (handler (class symbol))
  (remove-handler handler (find-class class)))

(defclass subject (entity handler-container)
  ((event-loops :initarg :event-loops :initform NIL :accessor event-loops))
  (:metaclass subject-class))

(defmethod banned-slots append ((object subject))
  '(event-loops))

(defmethod shared-initialize :after ((subject subject) slots &key)
  (regenerate-handlers subject))

(defmethod update-instance-for-redefined-class ((subject subject) aslots dslots plist &key)
  (declare (ignore aslots dslots plist))
  (regenerate-handlers subject))

(defmethod regenerate-handlers ((subject subject))
  (setf (handlers subject)
        (remove-if (lambda (handler)
                     (when (typep handler 'subject-handler)
                       (dolist (event-loop (event-loops subject))
                         (remove-handler handler event-loop))
                       T))
                   (handlers subject)))
  (loop for prototype in (effective-handlers (class-of subject))
        for handler = (make-instance
                       'subject-handler
                       :subject subject
                       :name (name prototype)
                       :event-type (event-type prototype)
                       :priority (priority prototype)
                       :delivery-function (delivery-function prototype))
        do (push handler (handlers subject))
           (dolist (event-loop (event-loops subject))
             (add-handler handler event-loop))))

(defmethod register :before ((subject subject) (loop event-loop))
  (when (find loop (event-loops subject))
    (error "~s is already registered on the event-loop ~s."
           subject loop)))

(defmethod register :after ((subject subject) (loop event-loop))
  (push loop (event-loops subject)))

(defmethod deregister :before ((subject subject) (loop event-loop))
  (unless (find loop (event-loops subject))
    (error "~s is not registered on the event-loop ~s."
           subject loop)))

(defmethod deregister :after ((subject subject) (loop event-loop))
  (setf (event-loops subject) (remove loop (event-loops subject))))

(defmacro define-subject (&environment env name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'subject)))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass subject-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defclass subject-handler (handler)
  ((subject :initarg :subject :accessor subject))
  (:default-initargs
   :subject (error "SUBJECT required.")))

(defmethod matches ((a subject-handler) (b subject-handler))
  (and (eq (subject a) (subject b))
       (eql (name a) (name b))))

(defmethod matches ((a subject-handler) (b handler))
  NIL)

(defmethod matches ((a handler) (b subject-handler))
  NIL)

(defmethod handle (event (handler subject-handler))
  (funcall (delivery-function handler) (subject handler) event))

(defmacro define-handler ((class name &optional (event-type name) (priority 0)) args &body body)
  (let ((event (first args))
        (args (rest args)))
    `(add-handler (make-instance
                   'subject-handler
                   :name ',name
                   :event-type ',event-type
                   :subject ',class
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
                   'subject-handler
                   :name ',name
                   :event-type ',event-type
                   :subject ',class
                   :priority ,priority
                   :delivery-function #',name)
                  ',class)))
