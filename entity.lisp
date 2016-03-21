#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric matches (a b))

(defmethod matches (a b)
  (equal a b))

(defclass entity ()
  ())

(defmethod matches ((a entity) b)
  (eql a b))

(defmethod matches (a (b entity))
  (matches b a))

(defclass named-entity (entity)
  ((name :initarg :name :reader name))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod matches ((a named-entity) b)
  (matches (name a) b))
