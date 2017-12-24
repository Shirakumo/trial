#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-subject-class (subject-class shader-entity-class)
  ())

(defmethod (setf effective-shaders) :after (shaders (class shader-subject-class))
  ;; Mark as obsolete
  (setf (class-redefinition-event-sent class) NIL)
  (make-instances-obsolete class))

(defclass shader-subject (subject shader-entity)
  ()
  (:metaclass shader-subject-class))

(defmacro define-shader-subject (&environment env name direct-superclasses direct-slots &rest options)
  (unless (find-if (lambda (c) (c2mop:subclassp (find-class c T env) 'shader-subject)) direct-superclasses)
    (setf direct-superclasses (append direct-superclasses (list 'shader-subject))))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass shader-subject-class) options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
