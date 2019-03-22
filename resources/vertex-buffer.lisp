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

(defmethod update-buffer-data ((buffer vertex-buffer) data &key (buffer-start 0) (data-start 0) count)
  (call-next-method buffer data :buffer-start buffer-start :data-start data-start :count count
                                :gl-type (element-type buffer)))

(defmethod resize-buffer ((buffer vertex-buffer) size &key (data (cffi:null-pointer)) (data-start 0))
  (call-next-method buffer size :data data :data-start data-start
                                :gl-type (element-type buffer)))

(defmethod allocate :before ((buffer buffer-object))
  (let ((buffer-data (buffer-data buffer)))
    (when (and (not (size buffer)) (vectorp buffer-data))
      (setf (size buffer) (* (length buffer-data)
                             (gl-type-size (element-type buffer)))))))
