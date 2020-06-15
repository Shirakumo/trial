#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-block (single-resource-asset)
  ())

(defmethod gl-source ((generator uniform-block))
  (load generator)
  (gl-source (resource generator T)))

(defmethod generate-resources ((generator uniform-block) input &key binding qualifiers)
  (ensure-instance (resource generator T) 'uniform-buffer
                   :binding binding
                   :qualifiers qualifiers
                   :struct-class (ensure-class input)))
