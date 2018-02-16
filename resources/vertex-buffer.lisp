#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-buffer (gl-resource)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (buffer-data :initarg :buffer-data :accessor buffer-data)
   (element-type :initarg :element-type :accessor element-type)
   (data-usage :initarg :data-usage :accessor data-usage)
   (size :initarg :size :initform NIL :accessor size))
  (:default-initargs
   :buffer-type :array-buffer
   :buffer-data (error "BUFFER-DATA required.")
   :element-type :float
   :data-usage :static-draw))

(defmethod initialize-instance :before ((asset vertex-buffer) &key buffer-type element-type data-usage)
  (check-vertex-buffer-type buffer-type)
  (check-vertex-buffer-element-type element-type)
  (check-vertex-buffer-data-usage data-usage))

(defmethod destructor ((buffer vertex-buffer))
  (let ((vbo (gl-name buffer)))
    (lambda () (gl:delete-buffers (list vbo)))))

(defmethod allocate ((buffer vertex-buffer))
  (with-slots (element-type buffer-type buffer-data data-usage) buffer
    (let* ((size (or (size buffer) (length buffer-data)))
           (bytes (* size (cffi:foreign-type-size element-type)))
           (vbo (gl:gen-buffer)))
      (with-cleanup-on-failure (offload vbo)
        (flet ((bind-data (buffer-data)
                 (gl:bind-buffer buffer-type vbo)
                 (unwind-protect
                      (%gl:buffer-data buffer-type bytes buffer-data data-usage)
                   (gl:bind-buffer buffer-type 0)))))
        (etypecase buffer-data
          (vector
           (if (and #+sbcl T #-sbcl NIL
                    (find (array-element-type buffer-data)
                          '(single-float double-float
                            (unsigned-byte 8)
                            (unsigned-byte 32) (signed-byte 32))
                          :test 'equal))
               ;; Arrays that already fit types can be shared directly on SBCL.
               (cffi:with-pointer-to-vector-data (buffer-data buffer-data)
                 (bind-data buffer-data))
               (cffi:with-foreign-object (array element-type size)
                 (loop for i from 0 for el across buffer-data
                       do (setf (cffi:mem-aref array i) (gl-coerce el element-type)))
                 (bind-data array))))
          (static-vector
           (bind-data (static-vector-pointer buffer-data)))
          (cffi:foreign-pointer
           (bind-data buffer-data)))
        (setf (size buffer) size)
        (setf (data-pointer buffer) vbo)))))
