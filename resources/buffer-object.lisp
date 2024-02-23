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
    (format stream "~a ~a~:[~; ALLOCATED~]" (buffer-type buffer) (data-usage buffer) (allocated-p buffer))))

(defun update-buffer-data/ptr (buffer data octets &optional (buffer-start 0))
  (let ((buffer-type (buffer-type buffer)))
    #-elide-buffer-access-checks
    (when (< (size buffer) (+ buffer-start octets))
      (error "Attempting to store ~d bytes of data at offset ~d in a buffer of size ~d."
             octets buffer-start (size buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:buffer-sub-data buffer-type buffer-start octets data)
      (gl:bind-buffer buffer-type 0))))

(defun download-buffer-data/ptr (buffer data octets &optional (buffer-start 0))
  (let ((buffer-type (buffer-type buffer)))
    #-elide-buffer-access-checks
    (when (< (size buffer) (+ buffer-start octets))
      (error "Attempting to read ~d bytes of data at offset ~d from a buffer of size ~d."
             octets buffer-start (size buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:get-buffer-sub-data buffer-type buffer-start octets data)
      (gl:bind-buffer buffer-type 0))))

(defun resize-buffer-data/ptr (buffer octets &optional (data (cffi:null-pointer)))
  (let ((buffer-type (buffer-type buffer)))
    (gl:bind-buffer buffer-type (gl-name buffer))
    (unwind-protect
         (%gl:buffer-data buffer-type octets data (data-usage buffer))
      (gl:bind-buffer buffer-type 0))
    (setf (size buffer) octets)))

(defmethod update-buffer-data ((buffer buffer-object) (data (eql T)) &rest args)
  (apply #'update-buffer-data buffer (buffer-data buffer) args))

(defmethod update-buffer-data ((buffer buffer-object) data &key (buffer-start 0) (data-start 0) count)
  (mem:with-memory-region (region data :offset data-start)
    #-elide-buffer-access-checks
    (when (and count (< (memory-region-size region) count))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             count data (memory-region-size region)))
    (update-buffer-data/ptr buffer (memory-region-pointer region) (or count (memory-region-size region)) buffer-start)))

(defmethod download-buffer-data ((buffer buffer-object) (data (eql T)) &rest args)
  (apply #'download-buffer-data buffer (buffer-data buffer) args))

(defmethod download-buffer-data ((buffer buffer-object) data &key (buffer-start 0) (data-start 0) count)
  (mem:with-memory-region (region data :offset data-start)
    #-elide-buffer-access-checks
    (when (and count (< (memory-region-size region) count))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             count data (memory-region-size region)))
    (download-buffer-data/ptr buffer (memory-region-pointer region) (or count (memory-region-size region)) buffer-start)))

(defmethod resize-buffer-data ((buffer buffer-object) (size (eql T)) &key (data (buffer-data buffer)) (data-start 0))
  (mem:with-memory-region (region data :offset data-start)
    (resize-buffer-data/ptr buffer (memory-region-size region) (memory-region-pointer region))))

(defmethod resize-buffer-data ((buffer buffer-object) size &key data (data-start 0))
  (mem:with-memory-region (region (or data (cffi:null-pointer)) :offset data-start)
    #-elide-buffer-access-checks
    (when (and size (not (cffi:null-pointer-p (memory-region-pointer region))) (< (memory-region-size region) size))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             size data (memory-region-size region)))
    (resize-buffer-data/ptr buffer size (memory-region-pointer region))))

(defmethod allocate ((buffer buffer-object))
  (assert (not (null (size buffer))))
  (let ((vbo (gl:gen-buffer))
        (buffer-data (buffer-data buffer)))
    (with-cleanup-on-failure (progn (gl:delete-buffers (list vbo))
                                    (setf (data-pointer buffer) NIL))
      (setf (data-pointer buffer) vbo)
      (v:debug :trial.resource "Allocating ~d KB buffer." (ceiling (size buffer) 1024))
      (resize-buffer-data buffer (size buffer) :data buffer-data))))

(defmethod deallocate ((buffer buffer-object))
  (gl:delete-buffers (list (gl-name buffer))))

(defmethod unload ((buffer buffer-object))
  (maybe-free-static-vector (buffer-data buffer))
  (setf (buffer-data buffer) NIL))

(defmethod activate ((buffer buffer-object))
  (gl:bind-buffer (buffer-type buffer) (gl-name buffer)))

(defmethod deactivate ((buffer buffer-object))
  (gl:bind-buffer (buffer-type buffer) 0))
