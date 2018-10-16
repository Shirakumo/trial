#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-buffer (buffer-object)
  ((element-type :initarg :element-type :accessor element-type))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float))

(defmethod initialize-instance :before ((buffer vertex-buffer) &key element-type)
  (check-vertex-buffer-element-type element-type))

(defmethod update-buffer-data ((buffer vertex-buffer) data &rest args)
  (let ((element-type (getf args :element-type)))
    (when (and element-type (not (equal element-type (element-type buffer))))
      (error "Cannot update vertex buffer of element type ~s with type ~s."
             (element-type buffer) element-type)))
  (apply #'call-next-method buffer data :element-type (element-type buffer) args))

(defmethod allocate :before ((buffer buffer-object))
  (let ((buffer-data (buffer-data buffer)))
    (when (and (not (size buffer)) (vectorp buffer-data))
      (setf (size buffer) (* (length buffer-data)
                             (gl-type-size (element-type buffer)))))))
