#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-buffer (gl-asset buffer-object)
  ((qualifiers :initarg :qualifiers :accessor qualifiers)
   (binding :initarg :binding :accessor binding)
   (struct :accessor struct))
  (:default-initargs
   :buffer-type :uniform-buffer
   :qualifiers ()))

(defmethod initialize-instance :after ((buffer uniform-buffer) &key name binding)
  (unless binding
    (setf (binding buffer) (cffi:translate-underscore-separated-name name))))

(defmethod reinitialize-instance :before ((buffer uniform-buffer) &key input)
  (when (and (not (equal input (input buffer)))
             (allocated-p buffer))
    (c2mop:remove-dependent (find-class (input buffer)) buffer)
    (c2mop:add-dependent (find-class input) buffer)))

(defmethod reinitialize-instance :after ((buffer uniform-buffer) &key)
  (when (allocated-p buffer)
    (c2mop:update-dependent (find-class (input buffer)) buffer)))

;;; FIXME: we update the buffer just fine, but what about the shader programs?
(defmethod c2mop:update-dependent ((class gl-struct-class) (buffer uniform-buffer) &rest _)
  (declare (ignore _))
  (when (allocated-p buffer)
    ;; FIXME: This currently zeroes out the data.
    ;;        We might be able to fix things up better and retain old values by copying across.
    (let ((new-size (buffer-field-size (input buffer) T 0)))
      (when (/= new-size (size buffer))
        (setf (size buffer) new-size)
        (let ((old (buffer-data buffer))
              (new (make-static-vector new-size :initial-element 0)))
          (maybe-free-static-vector old)
          (setf (buffer-data buffer) new)
          (resize-buffer buffer new-size :data new))))))

(defmethod (setf buffer-data) :after (data (buffer uniform-buffer))
  (setf (struct buffer) (make-instance (input buffer) :storage-ptr (static-vector-pointer data))))

(defmethod gl-type ((buffer uniform-buffer))
  (gl-type (find-class (input buffer))))

(defmethod struct-fields ((buffer uniform-buffer))
  (struct-fields (find-class (input buffer))))

(defmethod gl-source ((buffer uniform-buffer))
  `(glsl-toolkit:shader
    ,@(loop for dependent in (compute-dependent-types buffer)
            collect (gl-source (find-class dependent)))
    (glsl-toolkit:interface-declaration
     (glsl-toolkit:type-qualifier
      (glsl-toolkit:layout-qualifier
       (glsl-toolkit:layout-qualifier-id ,(layout-standard (find-class (input buffer)))))
      :uniform
      ,@(qualifiers buffer))
     ,(gl-type buffer)
     ,(if (binding buffer)
          `(glsl-toolkit:instance-name ,(binding buffer))
          'glsl-toolkit:no-value)
     ,@(mapcar #'gl-source (struct-fields buffer)))))

(defmethod compute-dependent-types ((buffer uniform-buffer))
  (compute-dependent-types (input buffer)))

(defmethod load ((buffer uniform-buffer))
  (unless (size buffer)
    (setf (size buffer) (buffer-field-size (input buffer) T 0))
    (setf (buffer-data buffer) (make-static-vector (size buffer) :initial-element 0)))
  (allocate buffer))

(defmethod allocate :after ((buffer uniform-buffer))
  (c2mop:add-dependent (find-class (input buffer)) buffer))

(defmethod deallocate :after ((buffer uniform-buffer))
  (c2mop:remove-dependent (find-class (input buffer)) buffer)
  (maybe-free-static-vector (buffer-data buffer))
  (setf (size buffer) NIL)
  (setf (buffer-data buffer) NIL))

(defmethod bind ((buffer uniform-buffer) (program shader-program) (binding-point integer))
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (let ((index (gl:get-uniform-block-index (gl-name program) (gl-type buffer))))
    (%gl:uniform-block-binding (gl-name program) index binding-point)
    (%gl:bind-buffer-base :uniform-buffer binding-point (gl-name buffer))))

(defmethod update-buffer-data ((buffer uniform-buffer) (data (eql T)) &key)
  (update-buffer-data/ptr buffer (static-vector-pointer (buffer-data buffer)) (size buffer)))

(defmacro with-buffer-tx ((struct buffer) &body body)
  (let ((bufferg (gensym "BUFFER")))
    `(let* ((,bufferg ,buffer)
            (,struct (struct ,bufferg)))
       (multiple-value-prog1
           (progn ,@body)
         (with-context (*context*)
           (update-buffer-data ,bufferg T))))))
