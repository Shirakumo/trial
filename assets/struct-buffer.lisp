#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass struct-buffer (asset buffer-object)
  ((struct :accessor struct))
  (:default-initargs
   :data-usage :stream-draw))

(defmethod print-object ((buffer struct-buffer) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a/~a ~a ~a"
            (when (pool buffer) (name (pool buffer))) (name buffer)
            (buffer-type buffer) (data-usage buffer))))

(defmethod coerce-asset-input ((asset struct-buffer) (input symbol))
  (find-class input))

(defmethod reinitialize-instance :before ((buffer struct-buffer) &key input)
  (when (and (not (equal input (input buffer)))
             (allocated-p buffer))
    (c2mop:remove-dependent (find-class (input buffer)) buffer)
    (c2mop:add-dependent (find-class input) buffer)))

(defmethod reinitialize-instance :after ((buffer struct-buffer) &key)
  (when (allocated-p buffer)
    (c2mop:update-dependent (input* buffer) buffer)))

(defmethod buffer-field-size ((buffer struct-buffer) standard base)
  (buffer-field-size (input* buffer) standard 0))

;;; FIXME: we update the buffer just fine, but what about the shader programs?
(defmethod c2mop:update-dependent ((class gl-struct-class) (buffer struct-buffer) &rest _)
  (declare (ignore _))
  (when (buffer-data buffer)
    ;; FIXME: This currently zeroes out the data.
    ;;        We might be able to fix things up better and retain old values by copying across.
    (let ((new-size (buffer-field-size buffer T 0)))
      (when (/= new-size (size buffer))
        (setf (size buffer) new-size)
        (let ((old (buffer-data buffer))
              (new (make-static-vector new-size :initial-element 0)))
          (maybe-free-static-vector old)
          (setf (buffer-data buffer) new)
          (when (allocated-p buffer)
            (resize-buffer buffer new-size :data new)))))))

(defmethod (setf buffer-data) :after (data (buffer struct-buffer))
  (setf (struct buffer) (make-instance (input buffer) :storage-ptr (static-vector-pointer data))))

(defmethod gl-type ((buffer struct-buffer))
  (gl-type (input* buffer)))

(defmethod struct-fields ((buffer struct-buffer))
  (struct-fields (input* buffer)))

(defmethod compute-dependent-types ((buffer struct-buffer))
  (compute-dependent-types (input* buffer)))

(defmethod load ((buffer struct-buffer))
  (unless (size buffer)
    (setf (size buffer) (buffer-field-size buffer T 0))
    (setf (buffer-data buffer) (make-static-vector (size buffer) :initial-element 0)))
  (allocate buffer))

(defmethod allocate :after ((buffer struct-buffer))
  (c2mop:add-dependent (input* buffer) buffer))

(defmethod deallocate :after ((buffer struct-buffer))
  (c2mop:remove-dependent (input* buffer) buffer)
  (maybe-free-static-vector (buffer-data buffer))
  (setf (size buffer) NIL)
  (setf (buffer-data buffer) NIL)
  (slot-makunbound buffer 'struct))

(defmethod update-buffer-data ((buffer struct-buffer) (data (eql T)) &key)
  (update-buffer-data/ptr buffer (static-vector-pointer (buffer-data buffer)) (size buffer)))

(defvar *buffers-in-tx* ())
(defmacro with-buffer-tx ((struct buffer) &body body)
  (let ((bufferg (gensym "BUFFER")))
    `(let ((,bufferg ,buffer))
       (multiple-value-prog1
           (let ((*buffers-in-tx* (list* ,bufferg *buffers-in-tx*))
                 (,struct (struct ,bufferg)))
             ,@body)
         (unless (find ,bufferg *buffers-in-tx*)
           (with-context (*context*)
             (update-buffer-data ,bufferg T)))))))
