#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-subject-class (subject-class shader-entity-class)
  ())

(defclass shader-subject (subject shader-entity)
  ()
  (:metaclass shader-subject-class))

(defmacro define-shader-subject (&environment env name direct-superclasses direct-slots &rest options)
  (setf direct-superclasses (append direct-superclasses (list 'shader-subject)))
  (unless (find :metaclass options :key #'first)
    (push '(:metaclass shader-subject-class) options))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))
