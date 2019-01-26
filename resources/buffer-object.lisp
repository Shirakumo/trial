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

;; ELEMENT-TYPE is forced to (UNSIGNED-BYTE 8) for data pointers.
;; DATA-START and DATA-END are in units of the size of ELEMENT-TYPE.
;; BUFFER-START and BUFFER-END are always in (UNSIGNED-BYTE 8) units.
(defgeneric update-buffer-data (buffer data &key data-start data-end buffer-start buffer-end element-type))

(defmethod update-buffer-data ((buffer buffer-object) data &key data-start data-end buffer-start buffer-end element-type)
  (declare (ignore element-type))
  (etypecase data
    (cffi:foreign-pointer
     (with-slots (buffer-type data-usage size) buffer
       (gl:bind-buffer buffer-type (gl-name buffer))
       (unwind-protect
            (let ((pointer (if data-start (cffi:inc-pointer data data-start) data)))
              (if (or buffer-start buffer-end)
                  (%gl:buffer-sub-data buffer-type (or buffer-start 0) (- (or data-end buffer-end size) data-start) pointer)
                  (%gl:buffer-data buffer-type size pointer data-usage)))
         (gl:bind-buffer buffer-type 0))))))

(defmethod update-buffer-data ((buffer buffer-object) (data null) &rest args)
  (apply #'update-buffer-data buffer (cffi:null-pointer) :data-start 0 args))

(defmethod update-buffer-data ((buffer buffer-object) (data real) &key data-start data-end buffer-start buffer-end element-type)
  (declare (ignore data-start data-end buffer-end))
  (let ((element-type (or element-type
                          (etypecase data
                            (single-float :float)
                            (double-float :double)
                            (T :int))))
        (buffer-start (or buffer-start 0))
        (buffer-end (+ buffer-start (gl-type-size (element-type buffer)))))
    (cffi:with-foreign-object (data element-type 1)
      (setf (cffi:mem-ref data element-type) (gl-coerce data element-type))
      (update-buffer-data buffer data :buffer-start buffer-start :buffer-end buffer-end))))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data mat2) &rest args)
  (apply #'update-buffer-data buffer (marr buffer-data) args))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data mat3) &rest args)
  (apply #'update-buffer-data buffer (marr buffer-data) args))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data mat4) &rest args)
  (apply #'update-buffer-data buffer (marr buffer-data) args))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data matn) &rest args)
  (apply #'update-buffer-data buffer (marr buffer-data) args))

;; FIXME: Directly fill a CFFI array to bypass unneeded calculations in the
;;        generic case variant
(defmethod update-buffer-data ((buffer buffer-object) (data vec2) &rest args)
  (let ((arr (make-static-vector 2 :element-type '#.3d-vectors::*float-type*)))
    (with-unwind-protection
        (maybe-free-static-vector arr)
      (setf (aref arr 0) (vx2 data))
      (setf (aref arr 1) (vy2 data))
      (apply #'update-buffer-data buffer arr args))))

(defmethod update-buffer-data ((buffer buffer-object) (data vec3) &rest args)
  (let ((arr (make-static-vector 3 :element-type '#.3d-vectors::*float-type*)))
    (with-unwind-protection
        (maybe-free-static-vector arr)
      (setf (aref arr 0) (vx3 data))
      (setf (aref arr 1) (vy3 data))
      (setf (aref arr 2) (vz3 data))
      (apply #'update-buffer-data buffer arr args))))

(defmethod update-buffer-data ((buffer buffer-object) (data vec4) &rest args)
  (let ((arr (make-static-vector 4 :element-type '#.3d-vectors::*float-type*)))
    (with-unwind-protection
        (maybe-free-static-vector arr)
      (setf (aref arr 0) (vx4 data))
      (setf (aref arr 1) (vy4 data))
      (setf (aref arr 2) (vz4 data))
      (setf (aref arr 3) (vw4 data))
      (apply #'update-buffer-data buffer arr args))))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data vector) &key (data-start 0) (data-end (length buffer-data)) buffer-start buffer-end element-type)
  (let* ((element-type (or element-type (cl-type->gl-type (array-element-type buffer-data))))
         (element-size (gl-type-size element-type))
         (buffer-start (when buffer-start (* buffer-start element-size)))
         (buffer-end (when buffer-end (* buffer-end element-size))))
    (cond ((static-vector-p buffer-data)
           (update-buffer-data buffer (static-vector-pointer buffer-data)
                               :data-start (* data-start element-size)
                               :data-end (* data-end element-size)
                               :buffer-start buffer-start
                               :buffer-end buffer-end))
          ((and #+sbcl T #-sbcl NIL
                (typep buffer-data 'simple-array)
                (find (array-element-type buffer-data) *native-array-element-types* :test 'equal))
           (sb-sys:with-pinned-objects (buffer-data)
             (update-buffer-data buffer (sb-sys:vector-sap buffer-data)
                                 :data-start (* data-start element-size)
                                 :data-end (* data-end element-size)
                                 :buffer-start buffer-start
                                 :buffer-end buffer-end)))
          (T
           (let* ((data-size (or (size buffer) (- buffer-end (or buffer-start 0))))
                  (elements (min (- data-end data-start) data-size)))
             (cffi:with-foreign-object (pointer element-type data-size)
               (loop for i from 0 below elements
                     do (setf (cffi:mem-aref pointer element-type i)
                              (gl-coerce (aref buffer-data (+ i data-start)) element-type)))
               ;; Null the rest
               (loop for i from elements below data-size
                     do (setf (cffi:mem-aref pointer element-type i)
                              (gl-coerce 0 element-type)))
               (update-buffer-data buffer pointer
                                   :data-start 0
                                   :data-end (* data-size element-size)
                                   :buffer-start buffer-start
                                   :buffer-end buffer-end)))))))

(defmethod allocate ((buffer buffer-object))
  (let ((vbo (gl:gen-buffer))
        (buffer-data (buffer-data buffer)))
    (with-cleanup-on-failure (progn (gl:delete-buffers (list vbo))
                                    (setf (data-pointer buffer) NIL))
      (setf (data-pointer buffer) vbo)
      (assert (not (null (size buffer))))
      (update-buffer-data buffer buffer-data))))
