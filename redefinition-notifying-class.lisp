#|
 This file is a part of trial
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass redefinition-notifying-class (standard-class)
  ((listeners :initform () :accessor %class-redefinition-listeners)))

(defmethod c2mop:validate-superclass ((class redefinition-notifying-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass redefinition-notifying-class))
  T)

(defmethod c2mop:validate-superclass ((class redefinition-notifying-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class redefinition-notifying-class) (superclass redefinition-notifying-class))
  T)

(defmethod c2mop:finalize-inheritance :around ((class redefinition-notifying-class))
  (call-next-method)
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (notify-class-redefinition class class))

(defmethod notify-class-redefinition ((class redefinition-notifying-class) redef)
  (loop for pointer in (%class-redefinition-listeners class)
        for listener = (tg:weak-pointer-value pointer)
        when listener do (with-simple-restart (continue "Ignore redefinition listener.")
                           (notify-class-redefinition listener redef))))

(defmethod class-redefinition-listeners ((class symbol))
  (class-redefinition-listeners (find-class class)))

(defmethod (setf class-redefinition-listeners) (value (class symbol))
  (setf (class-redefinition-listeners (find-class class)) value))

(defmethod class-redefinition-listeners ((class redefinition-notifying-class))
  (loop for pointer in (%class-redefinition-listeners class)
        for listener = (tg:weak-pointer-value pointer)
        when listener collect listener))

(defmethod (setf class-redefinition-listeners) (listeners (class redefinition-notifying-class))
  (setf (%class-redefinition-listeners class)
        (loop for listener in listeners
              for pointer = (tg:make-weak-pointer listener)
              collect listener)))

(defmethod add-class-redefinition-listener (listener (class redefinition-notifying-class))
  (unless (find listener (%class-redefinition-listeners class) :key #'tg:weak-pointer-value)
    (push (tg:make-weak-pointer listener) (%class-redefinition-listeners class)))
  listener)

(defmethod remove-class-redefinition-listener (listener (class redefinition-notifying-class))
  (setf (%class-redefinition-listeners class)
        (delete listener (%class-redefinition-listeners class) :key #'tg:weak-pointer-value))
  listener)
