#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass buffer-object (gl-resource)
  ((buffer-type :initarg :buffer-type :initform (error "BUFFER-TYPE required.") :accessor buffer-type)
   (buffer-data :initarg :buffer-data :initform NIL :accessor buffer-data)
   (data-usage :initarg :data-usage :initform :static-draw :accessor data-usage)
   (size :initarg :size :initform NIL :accessor size)))

(defmethod initialize-instance :before ((buffer buffer-object) &key buffer-type data-usage)
  (when buffer-type (check-buffer-object-type buffer-type))
  (when data-usage (check-buffer-object-data-usage data-usage)))

(defmethod print-object ((buffer buffer-object) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a ~a" (buffer-type buffer) (data-usage buffer))))

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

(defun resize-buffer/ptr (buffer size &optional (data (cffi:null-pointer)))
  (let ((buffer-type (buffer-type buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:buffer-data buffer-type size data (data-usage buffer))
      (gl:bind-buffer buffer-type 0))
    (setf (size buffer) size)))

(defmethod update-buffer-data ((buffer buffer-object) (data (eql T)) &rest args)
  (apply #'update-buffer-data buffer (buffer-data buffer) args))

(defmethod update-buffer-data ((buffer buffer-object) data &key (buffer-start 0) (data-start 0) count gl-type)
  (with-data-ptr (ptr data-size data :offset data-start :gl-type gl-type)
    #-elide-buffer-access-checks
    (when (and count (< data-size count))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             count data data-size))
    (update-buffer-data/ptr buffer ptr (or count data-size) buffer-start)))

(defmethod resize-buffer ((buffer buffer-object) size &key data (data-start 0) gl-type)
  (with-data-ptr (ptr data-size (or data (cffi:null-pointer)) :offset data-start :gl-type gl-type)
    #-elide-buffer-access-checks
    (when (and size (< data-size size))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             size data data-size))
    (resize-buffer/ptr buffer size ptr)))

(defmethod allocate ((buffer buffer-object))
  (let ((vbo (gl:gen-buffer))
        (buffer-data (buffer-data buffer)))
    (with-cleanup-on-failure (progn (gl:delete-buffers (list vbo))
                                    (setf (data-pointer buffer) NIL))
      (setf (data-pointer buffer) vbo)
      (assert (not (null (size buffer))))
      (resize-buffer buffer (size buffer) :data buffer-data))))

(defmethod deallocate ((buffer buffer-object))
  (gl:delete-buffers (list (gl-name buffer))))

(defmethod unload ((buffer buffer-object))
  (maybe-free-static-vector (buffer-data buffer))
  (setf (buffer-data buffer) NIL))
