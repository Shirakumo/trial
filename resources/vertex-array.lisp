#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-array (gl-resource)
  ((size :initarg :size :initform NIL :accessor size)
   (bindings :initarg :bindings :accessor bindings)
   (vertex-form :initarg :vertex-form :accessor vertex-form))
  (:default-initargs
   :bindings (error "BINDINGS required.")
   :vertex-form :triangles))

(defmethod print-object ((array vertex-array) stream)
  (print-unreadable-object (array stream :type T :identity T)
    (format stream "~@[~a~]" (size array))))

(defmethod destructor ((array vertex-array))
  (let ((vao (gl-name array)))
    (lambda () (when vao (gl:delete-vertex-arrays (list vao))))))

(defmethod dependencies ((array vertex-array))
  (mapcar #'unlist (bindings array)))

(defmethod allocate ((array vertex-array))
  (let ((vao (gl:gen-vertex-array)))
    (with-cleanup-on-failure (gl:delete-vertex-arrays (list vao))
      (gl:bind-vertex-array vao)
      (unwind-protect
           (loop for binding in (bindings array)
                 for i from 0
                 do (destructuring-bind (buffer &key (index i)
                                                     (size 3)
                                                     (stride 0)
                                                     (offset 0)
                                                     (normalize NIL)
                                                     (instancing 0)
                                                     (type))
                        (enlist binding)
                      (check-allocated buffer)
                      (gl:bind-buffer (buffer-type buffer) (gl-name buffer))
                      (ecase (buffer-type buffer)
                        (:element-array-buffer
                         (unless (size array)
                           (setf (size array) (/ (size buffer) (gl-type-size (element-type buffer)))))
                         (decf i))
                        (:array-buffer
                         (ecase (or type (element-type buffer))
                           ((:half-float :float :fixed)
                            (gl:vertex-attrib-pointer index size (element-type buffer) normalize stride offset))
                           ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
                            (gl:vertex-attrib-ipointer index size (element-type buffer) stride offset))
                           (:double
                            (%gl:vertex-attrib-lpointer index size (element-type buffer) stride offset)))
                         (gl:enable-vertex-attrib-array index)
                         (%gl:vertex-attrib-divisor index instancing)))))
        (gl:bind-vertex-array 0)
        (setf (data-pointer array) vao)))))
