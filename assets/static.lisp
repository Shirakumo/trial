#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass static (single-resource-asset)
  ())

(defmethod coerce-asset-input ((static static) input)
  (check-type input resource)
  input)

(defmethod generate-resources ((generator static) (input resource) &key)
  input)
