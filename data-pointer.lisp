#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun free-data (data)
  (etypecase data
    (cffi:foreign-pointer
     (cffi:foreign-free data))
    (vector
     (maybe-free-static-vector data))
    (mem:memory-region
     (mem:deallocate T data))
    (cons
     (mapc #'free-data data))))

(defmethod mem:call-with-memory-region (function (data vec2) &key (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 2)
    (setf (cffi:mem-aref ptr :float 0) (vx2 data))
    (setf (cffi:mem-aref ptr :float 1) (vy2 data))
    (let ((region (memory-region ptr (* 2 4))))
      (declare (dynamic-extent region))
      (funcall function region))))

(defmethod mem:call-with-memory-region (function (data vec3) &key (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 3)
    (setf (cffi:mem-aref ptr :float 0) (vx3 data))
    (setf (cffi:mem-aref ptr :float 1) (vy3 data))
    (setf (cffi:mem-aref ptr :float 2) (vz3 data))
    (let ((region (memory-region ptr (* 3 4))))
      (declare (dynamic-extent region))
      (funcall function region))))

(defmethod mem:call-with-memory-region (function (data vec4) &key (offset 0))
  #-elide-buffer-access-checks
  (when (/= offset 0) (error "OFFSET must be zero for vectors."))
  (cffi:with-foreign-object (ptr :float 4)
    (setf (cffi:mem-aref ptr :float 0) (vx4 data))
    (setf (cffi:mem-aref ptr :float 1) (vy4 data))
    (setf (cffi:mem-aref ptr :float 2) (vz4 data))
    (setf (cffi:mem-aref ptr :float 3) (vw4 data))
    (let ((region (memory-region ptr (* 4 4))))
      (declare (dynamic-extent region))
      (funcall function region))))

(defmethod mem:call-with-memory-region (function (data mat2) &rest args)
  (apply #'mem:call-with-memory-region function (marr2 data) args))

(defmethod mem:call-with-memory-region (function (data mat3) &rest args)
  (apply #'mem:call-with-memory-region function (marr3 data) args))

(defmethod mem:call-with-memory-region (function (data mat4) &rest args)
  (apply #'mem:call-with-memory-region function (marr4 data) args))

(defmethod mem:call-with-memory-region (function (data matn) &rest args)
  (apply #'mem:call-with-memory-region function (marrn data) args))
