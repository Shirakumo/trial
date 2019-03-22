#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass buffer-object (gl-resource)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (buffer-data :initarg :buffer-data :accessor buffer-data)
   (data-usage :initarg :data-usage :accessor data-usage)
   (size :initarg :size :initform NIL :accessor size))
  (:default-initargs
   :buffer-type (error "BUFFER-TYPE required.")
   :data-usage :static-draw
   :buffer-data NIL))

(defmethod initialize-instance :before ((buffer buffer-object) &key buffer-type data-usage)
  (check-buffer-object-type buffer-type)
  (check-buffer-object-data-usage data-usage))

(defmethod print-object ((buffer buffer-object) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a ~a" (buffer-type buffer) (data-usage buffer))))

(defmethod destructor ((buffer buffer-object))
  (let ((vbo (gl-name buffer)))
    (lambda () (when vbo (gl:delete-buffers (list vbo))))))

(defun update-buffer-data/ptr (buffer data count &optional (buffer-start 0))
  (let ((buffer-type (buffer-type buffer)))
    #-elide-buffer-access-checks
    (when (< (size buffer) (+ buffer-start count))
      (error "Attempting to store ~d bytes of data at offset ~d in a buffer of size ~d."
             count buffer-start (size buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:buffer-sub-data buffer-type buffer-start count data)
      (gl:bind-buffer buffer-type 0))))

(defun resize-buffer (buffer size &optional (data (cffi:null-pointer)))
  (let ((buffer-type (buffer-type buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:buffer-data buffer-type size data (data-usage buffer))
      (gl:bind-buffer buffer-type 0))))

(defgeneric call-with-data-ptr (function data &optional offset))

(defmethod call-with-data-ptr (function data &optional (offset 0))
  #-elide-buffer-access-checks
  (unless (typep data 'cffi:foreign-pointer)
    (no-applicable-method #'call-with-data-ptr function data :offset offset))
  (funcall function (cffi:inc-pointer data offset) 0))

(defmethod call-with-data-ptr (function (data real) &optional (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for reals."))
  (let ((type (cl-type->gl-type (type-of data))))
    (cffi:with-foreign-object (ptr type)
      (setf (cffi:mem-ref ptr type) data)
      (funcall function ptr (gl-type-size type)))))

(defmethod call-with-data-ptr (function (data vec2) &optional (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 2)
    (setf (cffi:mem-aref ptr :float 0) (vx2 data))
    (setf (cffi:mem-aref ptr :float 1) (vy2 data))
    (funcall function ptr (g-type-size :vec2))))

(defmethod call-with-data-ptr (function (data vec3) &optional (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 3)
    (setf (cffi:mem-aref ptr :float 0) (vx3 data))
    (setf (cffi:mem-aref ptr :float 1) (vy3 data))
    (setf (cffi:mem-aref ptr :float 2) (vz3 data))
    (funcall function ptr (g-type-size :vec3))))

(defmethod call-with-data-ptr (function (data vec4) &optional (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 4)
    (setf (cffi:mem-aref ptr :float 0) (vx4 data))
    (setf (cffi:mem-aref ptr :float 1) (vy4 data))
    (setf (cffi:mem-aref ptr :float 2) (vz4 data))
    (setf (cffi:mem-aref ptr :float 3) (vw4 data))
    (funcall function ptr (g-type-size :vec4))))

(defmethod call-with-data-ptr (function (data mat2) &optional (offset 0))
  (call-with-data-ptr function (marr2 data) offset))

(defmethod call-with-data-ptr (function (data mat3) &optional (offset 0))
  (call-with-data-ptr function (marr3 data) offset))

(defmethod call-with-data-ptr (function (data mat4) &optional (offset 0))
  (call-with-data-ptr function (marr4 data) offset))

(defmethod call-with-data-ptr (function (data matn) &optional (offset 0))
  (call-with-data-ptr function (marrn data) offset))

(defmethod call-with-data-ptr (function (data vector) &optional (offset 0))
  (let* ((type (cl-type->gl-type element-type))
         (type-size (gl-type-size type))
         (offset (* offset type-size))
         (size (- (* (length data) type-size) offset)))
    (cond #+sbcl
          ((typep data 'simple-array)
           (sb-sys:with-pinned-objects (data)
             (funcall function (sb-sys:sap+ (sb-sys:vector-sap data) offset) size)))
          ((static-vector-p data)
           (funcall function (cffi:inc-pointer (static-vector-pointer data) offset) size))
          (T
           (cffi:with-foreign-array (ptr data type)
             (funcall function (cffi:inc-pointer ptr offset) size))))))

(defmacro with-data-ptr ((ptr size data &rest args &optional offset) &body body)
  (declare (ignore offset))
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,ptr ,size)
              (declare (type cffi:foreign-pointer ,ptr))
              (declare (type fixnum ,size))
              ,@body))
       (call-with-data-ptr #',thunk ,data ,@args))))

(defmethod update-buffer-data ((buffer buffer-object) data &key buffer-start data-start count)
  (with-data-ptr (ptr size data data-start)
    (update-buffer-data/ptr buffer ptr (or count size) buffer-start)))

(defmethod allocate ((buffer buffer-object))
  (let ((vbo (gl:gen-buffer))
        (buffer-data (buffer-data buffer)))
    (with-cleanup-on-failure (progn (gl:delete-buffers (list vbo))
                                    (setf (data-pointer buffer) NIL))
      (setf (data-pointer buffer) vbo)
      (assert (not (null (size buffer))))
      (with-data-ptr (ptr buffer-data)
        (resize-buffer vbo (size buffer) ptr)))))
