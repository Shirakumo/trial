#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass buffer-object (gl-resource)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (buffer-data :initarg :buffer-data :accessor buffer-data)
   (element-type :initarg :element-type :accessor element-type)
   (data-usage :initarg :data-usage :accessor data-usage)
   (size :initarg :size :initform NIL :accessor size))
  (:default-initargs
   :buffer-type (error "BUFFER-TYPE required.")
   :data-usage :static-draw
   :buffer-data NIL
   :element-type NIL))

(defmethod initialize-instance :before ((buffer buffer-object) &key buffer-type element-type data-usage)
  (check-buffer-object-type buffer-type)
  (check-buffer-object-element-type element-type)
  (check-buffer-object-data-usage data-usage))

(defmethod print-object ((buffer buffer-object) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a ~a ~a" (data-usage buffer) (buffer-type buffer) (element-type buffer))))

(defmethod destructor ((buffer buffer-object))
  (let ((vbo (gl-name buffer)))
    (lambda () (when vbo (gl:delete-buffers (list vbo))))))

(defgeneric update-buffer-data (buffer data &key data-start data-end buffer-start buffer-end))

(defmethod update-buffer-data ((buffer buffer-object) data &key data-start data-end buffer-start buffer-end)
  (etypecase data
    (cffi:foreign-pointer
     (with-slots (element-type buffer-type data-usage size) buffer
       (gl:bind-buffer buffer-type (gl-name buffer))
       (unwind-protect
            (let* ((pointer (if data-start
                                (cffi:inc-pointer data data-start)
                                data))
                   (element-size (cffi:foreign-type-size element-type))
                   (buffer-start (or buffer-start 0)))
              (if (or buffer-start buffer-end)
                  (%gl:buffer-sub-data buffer-type (or buffer-start 0) (- (or data-end buffer-end (* size element-size)) data-start) pointer)
                  (%gl:buffer-data buffer-type (* size element-size) pointer data-usage)))
         (gl:bind-buffer buffer-type 0))))))

(defmethod update-buffer-data ((buffer buffer-object) (data null) &key data-start data-end buffer-start buffer-end)
  (declare (ignore data-start data-end))
  (update-buffer-data buffer (cffi:null-pointer) :buffer-start buffer-start :buffer-end buffer-end))

(defmethod update-buffer-data ((buffer buffer-object) (buffer-data vector) &key (data-start 0) (data-end (length buffer-data)) buffer-start buffer-end)
  (let* ((element-type (element-type buffer))
         (element-size (cffi:foreign-type-size (element-type buffer)))
         (buffer-start (when buffer-start (* buffer-start element-size)))
         (buffer-end (when buffer-end (* buffer-end element-size))))
    (cond ((static-vector-p buffer-data)
           (update-buffer-data buffer (static-vector-pointer buffer-data)
                               :data-start (* data-start element-size)
                               :data-end (* data-end element-size)
                               :buffer-start buffer-start
                               :buffer-end buffer-end))
          ((and #+sbcl T #-sbcl NIL
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
      (when (and (not (size buffer)) (vectorp buffer-data))
        (setf (size buffer) (length buffer-data)))
      (assert (not (null (size buffer))))
      (update-buffer-data buffer buffer-data))))
