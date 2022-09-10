#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +action-set-states+ (make-hash-table :test 'eq))

(defclass action-set () ()) ;; marker-class
(defclass exclusive-action-set () ())

(defmethod (setf active-p) :after (value (set exclusive-action-set))
  (when value
    (dolist (other (c2mop:class-direct-subclasses (find-class 'exclusive-action-set)))
      (unless (eql (class-of set) other)
        (setf (active-p other) NIL)))))

(defun find-action-set (action)
  (flet ((direct-action-set (base)
           (loop for class in (c2mop:class-direct-superclasses base)
                 do (when (eql class (find-class 'action-set))
                      (return base)))))
    (or (direct-action-set action)
        (loop for class in (or (ignore-errors (c2mop:class-precedence-list action))
                               (c2mop:compute-class-precedence-list action))
              thereis (direct-action-set class))
        (find-class 'action))))

(defun action-set (action)
  (find-action-set (ensure-class action)))

(defun list-action-sets ()
  (c2mop:class-direct-subclasses (find-class 'action-set)))

(defun active-action-set ()
  (find T (list-action-sets) :key #'active-p))

(define-compiler-macro action-set (action &environment env)
  (if (constantp action env)
      `(load-time-value (find-action-set (ensure-class ,action)))
      `(find-action-set (ensure-class ,action))))

(defmacro define-action-set (name &optional superclasses)
  `(progn (defclass ,name (,@superclasses action-set)
            ())
          (defmethod active-p ((,name ,name))
            (or (gethash ',name +action-set-states+)
                (when (next-method-p)
                  (call-next-method))))
          (defmethod (setf active-p) (value (,name ,name))
            (setf (gethash ',name +action-set-states+) value))
          (defmethod active-p ((class (eql (find-class ',name))))
            (gethash ',name +action-set-states+))
          (defmethod (setf active-p) (value (class (eql (find-class ',name))))
            (setf (gethash ',name +action-set-states+) value))
          (c2mop:finalize-inheritance (find-class ',name))))

(defclass action (event)
  ((source-event :initarg :source-event :initform NIL :accessor source-event)))

(defmethod active-p ((action (eql (find-class 'action)))) T)
(defmethod active-p ((action action)) NIL)

(defclass analog-action (action)
  ((value :initarg :value :initform 0f0 :accessor value)))

(defclass directional-action (action)
  ((x :initarg :value :initform 0f0 :accessor x)
   (y :initarg :value :initform 0f0 :accessor y)))

(defclass spatial-action (action)
  ((x :initarg :value :initform 0f0 :accessor x)
   (y :initarg :value :initform 0f0 :accessor y)
   (z :initarg :value :initform 0f0 :accessor z)))

(defmacro define-action (name superclasses)
  `(progn
     (defclass ,name ,(append superclasses '(action))
       ())
     ,(if superclasses
          `(undefmethod active-p ((,name ,name)))
          `(defmethod active-p ((,name ,name)) T))
     ',name))
