#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric matches (a b))

(defmethod matches :around (a b)
  (or (eq a b)
      (call-next-method)))

(defmethod matches (a b)
  (equal a b))

(defclass entity (unit)
  ())

(defmethod matches ((a entity) b)
  (or (eql a b)
      (matches (name a) b)))

(defmethod matches (a (b entity))
  (matches b a))
