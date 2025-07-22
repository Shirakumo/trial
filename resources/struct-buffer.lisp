(in-package #:org.shirakumo.fraf.trial)

(defclass struct-buffer (buffer-object)
  ((data-usage :initform :dynamic-draw)
   (map-bits :initform '() :initarg :map-bits :accessor map-bits)))

(defmethod print-object ((buffer struct-buffer) stream)
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a ~a~:[~; ALLOCATED~]" (type-of (buffer-data buffer)) (data-usage buffer) (allocated-p buffer))))

(defmethod shared-initialize :after ((buffer struct-buffer) slots &key struct-class struct)
  (setf (buffer-data buffer) (cond (struct
                                    (etypecase struct
                                      ((or symbol class) (make-instance struct))
                                      ((or vector memory-region gl-struct) struct)))
                                   (struct-class
                                    (make-instance struct-class))
                                   (T
                                    (error "STRUCT or STRUCT-CLASS must be passed.")))))

(defmethod reinitialize-instance :after ((buffer struct-buffer) &key)
  (when (allocated-p buffer)
    (c2mop:update-dependent (struct-class buffer) buffer)))

(defmethod finalize :after ((buffer struct-buffer))
  (unless (generator buffer)
    (finalize (buffer-data buffer))))

(defmethod struct-class ((buffer struct-buffer))
  (type-of (buffer-data buffer)))

(defmethod buffer-field-size ((standard symbol) (buffer struct-buffer) base)
  (buffer-field-size standard (buffer-data buffer) base))

(defmethod buffer-field-size ((standard (eql T)) (buffer struct-buffer) base)
  (buffer-field-size (layout-standard buffer) buffer base))

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
          (resize-buffer-data buffer new-size :data (buffer-data buffer)))))))

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
  (let ((region (storage (buffer-data buffer))))
    (update-buffer-data/ptr buffer (memory-region-pointer region) (min (size buffer) (memory-region-size region)))))

(defmethod download-buffer-data ((buffer struct-buffer) (data (eql T)) &key)
  (let ((region (storage (buffer-data buffer))))
    (download-buffer-data/ptr buffer (memory-region-pointer region) (min (size buffer) (memory-region-size region)))))

(defmethod resize-buffer-data ((buffer struct-buffer) (size (eql T)) &key (data (storage (buffer-data buffer))) (data-start 0))
  (mem:with-memory-region (region data :offset data-start)
    (resize-buffer-data/ptr buffer (memory-region-size region) (memory-region-pointer region))))

#-elide-buffer-access-checks
(defmethod resize-buffer-data :before ((buffer struct-buffer) (octets integer) &key &allow-other-keys)
  (when (< (size buffer) (buffer-field-size T buffer 0))
    (error "Attempting to resize the struct buffer to ~d bytes, which is too small to hold the ~d bytes for its struct!"
           octets (buffer-field-size T buffer 0))))

(defmethod allocate ((buffer struct-buffer))
  (mem:with-memory-region (region (storage (buffer-data buffer)))
    (let ((vbo (gl:gen-buffer)))
      (with-cleanup-on-failure (progn (gl:delete-buffers (list vbo))
                                      (setf (data-pointer buffer) NIL))
        (setf (data-pointer buffer) vbo)
        (v:debug :trial.resource "Allocating ~d KB buffer." (ceiling (size buffer) 1024))
        (%gl:bind-buffer (buffer-type buffer) vbo)
        (%gl:buffer-storage
         (buffer-type buffer)
         (memory-region-size region)
         (memory-region-pointer region)
         (ecase (data-usage buffer)
           ((:dynamic-draw :stream-draw :static-draw)
            (list* :dynamic-storage :map-write (map-bits buffer)))
           ((:dynamic-read :stream-read :static-read)
            (list* :dynamic-storage :map-read (map-bits buffer)))
           ((:dynamic-copy :stream-copy :static-copy)
            (list* :dynamic-storage :map-read :map-write (map-bits buffer)))))))))

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
             (declare (dynamic-extent *buffers-in-tx*))
             ,@body)
         (when (and (or (eql :write ,updateg) (eql T ,updateg)) (not (find ,bufferg *buffers-in-tx*)))
           (update-buffer-data ,bufferg T))))))
