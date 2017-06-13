#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-buffer (asset)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (element-type :initarg :element-type :accessor element-type)
   (data-usage :initarg :data-usage :accessor data-usage)
   (size :initarg :size :accessor size))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float
   :data-usage :static-draw))

(defmethod initialize-instance :before ((asset vertex-buffer) &key buffer-type element-type data-usage)
  (check-vertex-buffer-type buffer-type)
  (check-vertex-buffer-element-type element-type)
  (check-vertex-buffer-data-usage data-usage))

(defmethod coerce-input ((asset vertex-buffer) (vector vector))
  vector)

(defmethod coerce-input ((asset vertex-buffer) (list list))
  (coerce list 'vector))

(defmethod coerce-input ((asset vertex-buffer) (number real))
  (make-array 1 :initial-element (float number)))

(defmethod coerce-input ((asset vertex-buffer) (pointer T))
  (check-type pointer (or cffi:foreign-pointer
                          static-vectors:static-vector))
  pointer)

(defmethod finalize-resource ((type (eql 'vertex-buffer)) resource)
  (gl:delete-buffers (list resource)))

(defun ensure-single-vector (inputs)
  (if (cdr inputs)
      (let ((output (make-array 0 :adjustable T :fill-pointer T)))
        (dolist (input inputs output)
          (loop for v across input do (vector-push-extend v output))))
      (first inputs)))

(defmethod load progn ((asset vertex-buffer))
  (let ((buffer-data (ensure-single-vector (coerced-inputs asset))))
    (with-slots (element-type buffer-type data-usage) asset
      (let ((buffer (gl:gen-buffer)))
        (setf (resource asset) buffer)
        (with-cleanup-on-failure (offload asset)
          (etypecase buffer-data
            (vector
             (let ((array (gl:alloc-gl-array element-type (length buffer-data))))
               (unwind-protect
                    (loop initially (gl:bind-buffer buffer-type buffer)
                          for i from 0
                          for el across buffer-data
                          do (setf (gl:glaref array i) (gl-coerce el element-type))
                          finally (gl:buffer-data buffer-type data-usage array))
                 (gl:free-gl-array array)
                 (gl:bind-buffer buffer-type 0))
               (setf (size asset) (length buffer-data))))
            (static-vectors:static-vector
             (let ((array (gl::make-gl-array-from-pointer (static-vectors:static-vector-pointer buffer-data)
                                                          element-type (size asset))))
               (gl:bind-buffer buffer-type buffer)
               (unwind-protect
                    (gl:buffer-data buffer-type data-usage array)
                 (gl:bind-buffer buffer-type 0))))
            (cffi:foreign-pointer
             (let ((array (gl::make-gl-array-from-pointer buffer-data element-type (size asset))))
               (gl:bind-buffer buffer-type buffer)
               (unwind-protect
                    (gl:buffer-data buffer-type data-usage array)
                 (gl:bind-buffer buffer-type 0))))))))))
