#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass buffer-block (single-resource-asset)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)))

(defmethod gl-source ((generator buffer-block))
  (load generator)
  (gl-source (resource generator T)))

(defmethod generate-resources ((generator buffer-block) input &rest args)
  (apply #'ensure-instance (resource generator T) (buffer-type generator) :struct input args))

(defclass uniform-block (buffer-block)
  ((buffer-type :initform 'uniform-buffer)))

(defclass shader-storage-block (buffer-block)
  ((buffer-type :initform 'shader-storage-buffer)))
