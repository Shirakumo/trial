#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass struct-buffer (buffer-object)
  ((data-usage :initform :dynamic-draw)))

(defmethod print-object ((buffer struct-buffer) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a ~a~:[~; ALLOCATED~]" (type-of (buffer-data buffer)) (data-usage buffer) (allocated-p buffer))))

(defmethod shared-initialize :after ((buffer struct-buffer) slots &key struct-class struct)
  (when struct-class
    (setf (buffer-data buffer) (make-instance struct-class)))
  (when struct
    (setf (buffer-data buffer) (etypecase struct
                                 ((or symbol class) (make-instance struct))
                                 ((or vector memory-region gl-struct) struct)))))

(defmethod reinitialize-instance :after ((buffer struct-buffer) &key)
  (when (allocated-p buffer)
    (c2mop:update-dependent (struct-class buffer) buffer)))

(defmethod finalize :after ((buffer struct-buffer))
  (unless (generator buffer)
    (finalize (buffer-data buffer))))

(defmethod struct-class ((buffer struct-buffer))
  (type-of (buffer-data buffer)))

(defmethod buffer-field-size ((standard symbol) (buffer struct-buffer) base)
  (buffer-field-size standard (buffer-data buffer) 0))

(defmethod buffer-field-size ((standard (eql T)) (buffer struct-buffer) base)
  (buffer-field-size (layout-standard buffer) buffer 0))

(defmethod (setf buffer-data) :before ((struct gl-struct) (buffer struct-buffer))
  (when (and (buffer-data buffer) (not (eq (class-of struct) (class-of (buffer-data buffer)))))
    (c2mop:remove-dependent (class-of (buffer-data buffer)) buffer)
    (c2mop:add-dependent (class-of struct) buffer)))

(defmethod (setf buffer-data) :around (object (buffer struct-buffer))
  (check-type object (or null gl-struct))
  (call-next-method))

(defmethod (setf buffer-data) :after ((object gl-struct) (buffer struct-buffer))
  (setf (size buffer) (buffer-field-size (layout-standard buffer) buffer 0)))

;;; FIXME: we update the buffer just fine, but what about the shader programs?
;;; FIXME: we currently only catch dependents for the top level class, not any of its includes!
;;; FIXME: we also do not ensure the new data stays sane at all!!
(defmethod c2mop:update-dependent ((class gl-struct-class) (buffer struct-buffer) &rest _)
  (declare (ignore _))
  (when (buffer-data buffer)
    (let ((new-size (buffer-field-size T buffer 0)))
      (when (/= new-size (size buffer))
        (setf (size buffer) new-size)
        (mem:reallocate T (buffer-data buffer) new-size)
        (when (allocated-p buffer)
          (resize-buffer buffer new-size :data (buffer-data buffer)))))))

(defmethod gl-type ((buffer struct-buffer))
  (gl-type (buffer-data buffer)))

(defmethod struct-fields ((buffer struct-buffer))
  (let ((*dynamic-context* (buffer-data buffer)))
    (mapcar #'gl-source (struct-fields (buffer-data buffer)))))

(defmethod layout-standard ((buffer struct-buffer))
  (layout-standard (buffer-data buffer)))

(defmethod compute-dependent-types ((buffer struct-buffer))
  (compute-dependent-types (buffer-data buffer)))

(defmethod allocate :after ((buffer struct-buffer))
  (c2mop:add-dependent (class-of (buffer-data buffer)) buffer))

(defmethod deallocate :after ((buffer struct-buffer))
  (c2mop:remove-dependent (class-of (buffer-data buffer)) buffer))

(defmethod update-buffer-data ((buffer struct-buffer) (data (eql T)) &key)
  (mem:with-memory-region (region (buffer-data buffer))
    (update-buffer-data/ptr buffer (memory-region-pointer region) (min (size buffer) (memory-region-size region)))))

(defmethod download-buffer-data ((buffer struct-buffer) (data (eql T)) &key)
  (mem:with-memory-region (region (buffer-data buffer))
    (download-buffer-data/ptr buffer (memory-region-pointer region) (min (size buffer) (memory-region-size region)))))

(defvar *buffers-in-tx* ())
(defmacro with-buffer-tx ((struct buffer &key (update :write)) &body body)
  (let ((bufferg (gensym "BUFFER"))
        (updateg (gensym "UPDATE")))
    `(let ((,bufferg ,buffer)
           (,updateg ,update))
       (when (and (or (eql :read ,updateg) (eql T ,updateg)) (not (find ,bufferg *buffers-in-tx*)))
         (download-buffer-data ,bufferg T))
       (multiple-value-prog1
           (let ((*buffers-in-tx* (list* ,bufferg *buffers-in-tx*))
                 (,struct (buffer-data ,bufferg)))
             ,@body)
         (when (and (or (eql :write ,updateg) (eql T ,updateg)) (not (find ,bufferg *buffers-in-tx*)))
           (update-buffer-data ,bufferg T))))))
