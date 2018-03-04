#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-array (gl-resource)
  ((size :initarg :size :initform NIL :accessor size)
   (bindings :initarg :bindings :accessor bindings))
  (:default-initargs
   :bindings (error "BINDINGS required.")))

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
                                                     (normalized NIL))
                        (enlist binding)
                      (check-allocated buffer)
                      (gl:bind-buffer (buffer-type buffer) (gl-name buffer))
                      (ecase (buffer-type buffer)
                        (:element-array-buffer
                         (unless (size array)
                           (setf (size array) (size buffer)))
                         (decf i))
                        (:array-buffer
                         (gl:vertex-attrib-pointer index size (element-type buffer) normalized stride offset)
                         (gl:enable-vertex-attrib-array index)))))
        (gl:bind-vertex-array 0)
        (setf (data-pointer array) vao)))))
