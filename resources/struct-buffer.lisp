#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass struct-buffer (buffer-object)
  ((data-usage :initform :stream-draw)
   (struct :accessor struct)
   (struct-class :accessor struct-class)))

(defmethod shared-initialize :after ((buffer struct-buffer) slots &key struct-class)
  (when struct-class
    (setf (struct-class buffer) (ensure-class struct-class))))

(defmethod reinitialize-instance :before ((buffer struct-buffer) &key struct-class)
  (when (and (not (equal struct-class (struct-class buffer)))
             (allocated-p buffer))
    (c2mop:remove-dependent (struct-class buffer) buffer)
    (c2mop:add-dependent (ensure-class struct-class) buffer)))

(defmethod reinitialize-instance :after ((buffer struct-buffer) &key)
  (when (allocated-p buffer)
    (c2mop:update-dependent (struct-class buffer) buffer)))

(defmethod buffer-field-size ((standard symbol) (buffer struct-buffer) base)
  (buffer-field-size standard (struct-class buffer) 0))

;;; FIXME: we update the buffer just fine, but what about the shader programs?
(defmethod c2mop:update-dependent ((class gl-struct-class) (buffer struct-buffer) &rest _)
  (declare (ignore _))
  (when (buffer-data buffer)
    ;; FIXME: This currently zeroes out the data.
    ;;        We might be able to fix things up better and retain old values by copying across.
    (let ((new-size (buffer-field-size T buffer 0)))
      (when (/= new-size (size buffer))
        (setf (size buffer) new-size)
        (let ((old (buffer-data buffer))
              (new (make-static-vector new-size :initial-element 0)))
          (maybe-free-static-vector old)
          (setf (buffer-data buffer) new)
          (when (allocated-p buffer)
            (resize-buffer buffer new-size :data new)))))))

(defmethod (setf buffer-data) :after (data (buffer struct-buffer))
  (setf (struct buffer) (make-instance (struct-class buffer) :storage-ptr (static-vector-pointer data))))

(defmethod gl-type ((buffer struct-buffer))
  (gl-type (struct-class buffer)))

(defmethod struct-fields ((buffer struct-buffer))
  (struct-fields (struct-class buffer)))

(defmethod compute-dependent-types ((buffer struct-buffer))
  (compute-dependent-types (struct-class buffer)))

(defmethod allocate :before ((buffer struct-buffer))
  (unless (size buffer)
    (setf (size buffer) (buffer-field-size T buffer 0))
    (setf (buffer-data buffer) (make-static-vector (size buffer) :initial-element 0))))

(defmethod allocate :after ((buffer struct-buffer))
  (c2mop:add-dependent (struct-class buffer) buffer))

(defmethod deallocate :after ((buffer struct-buffer))
  (c2mop:remove-dependent (struct-class buffer) buffer)
  (maybe-free-static-vector (buffer-data buffer))
  (setf (size buffer) NIL)
  (setf (buffer-data buffer) NIL)
  (slot-makunbound buffer 'struct))

(defmethod update-buffer-data ((buffer struct-buffer) (data (eql T)) &key)
  (update-buffer-data/ptr buffer (static-vector-pointer (buffer-data buffer)) (size buffer)))

(defvar *buffers-in-tx* ())
(defmacro with-buffer-tx ((struct buffer &key (update T)) &body body)
  (let ((bufferg (gensym "BUFFER")))
    `(let ((,bufferg ,buffer))
       (multiple-value-prog1
           (let ((*buffers-in-tx* (list* ,bufferg *buffers-in-tx*))
                 (,struct (struct ,bufferg)))
             ,@body)
         (when (and ,update (not (find ,bufferg *buffers-in-tx*)))
           (with-context (*context*)
             (update-buffer-data ,bufferg T)))))))
