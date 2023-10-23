(in-package #:org.shirakumo.fraf.trial)

(defclass memory (foreign-resource)
  ((size :initarg :size :initform NIL :accessor size)))

(defmethod print-object ((resource memory) stream)
  (print-unreadable-object (resource stream :type T :identity T)
    (format stream "~a bytes~:[~; ALLOCATED~]" (size resource) (allocated-p resource))))

(defmethod allocate ((resource memory))
  (setf (data-pointer resource) (cffi:foreign-alloc :uint8 :count (or (size resource)
                                                                      (error "SIZE required")))))

(defmethod deallocate ((resource memory))
  (cffi:foreign-free (data-pointer resource)))

(defmethod mem:to-memory-region ((resource memory))
  (mem:memory-region (data-pointer resource)
                     (size resource)))

(defmethod mem:call-with-memory-region ((function function) (resource memory) &key (start 0))
  (let ((mem (mem:memory-region (cffi:inc-pointer (data-pointer resource) start)
                                (- (size resource) start))))
    (declare (dynamic-extent mem))
    (funcall function mem)))
