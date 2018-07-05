#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-buffer (gl-asset buffer-object)
  ((layout :initarg :layout :accessor layout)
   (qualifiers :initarg :qualifiers :accessor qualifiers)
   (binding :initarg :binding :accessor binding)
   (offsets :initarg :offsets :initform NIL :accessor offsets))
  (:default-initargs
   :buffer-type :uniform-buffer
   :layout :shared
   :qualifiers ()))

(defmethod initialize-instance :after ((buffer uniform-buffer) &key name binding)
  (unless binding
    (setf (binding buffer) (cffi:translate-underscore-separated-name name))))

(defmethod gl-type ((buffer uniform-buffer))
  (gl-type (gl-struct (input buffer))))

(defmethod fields ((buffer uniform-buffer))
  (fields (gl-struct (input buffer))))

(defmethod gl-source ((buffer uniform-buffer))
  `(glsl-toolkit:interface-declaration
    (glsl-toolkit:type-qualifier
     ,@(when (layout buffer)
         `((glsl-toolkit:layout-qualifier
            ,@(loop for id in (enlist (layout buffer))
                    collect `(glsl-toolkit:layout-qualifier-id ,@(enlist id))))))
     :uniform
     ,@(qualifiers buffer))
    ,(gl-type buffer)
    ,(if (binding buffer)
         `(glsl-toolkit:instance-name ,(binding buffer))
         'glsl-toolkit:no-value)
    ,@(mapcar #'gl-source (fields buffer))))

(defmethod compute-offsets ((buffer uniform-buffer) (program shader-program))
  (let* ((struct (gl-struct (input buffer)))
         (index (gl:get-uniform-block-index (gl-name program) (gl-type struct)))
         (size (gl:get-active-uniform-block (gl-name program) index :uniform-block-data-size))
         (offsets (make-hash-table :test 'equal)))
    (labels ((gather-fields (struct prefix)
               (loop for field in (fields struct)
                     nconc (cond ((and (array-size field) (listp (gl-type field)))
                                  (loop for i from 0 below (array-size field)
                                        nconc (gather-fields (gl-struct (second (gl-type field)))
                                                             (format NIL "~a~a[~d]" prefix (gl-name field) i))))
                                 ((array-size field)
                                  (loop for i from 0 below (array-size field)
                                        collect (format NIL "~a~a[~d]" prefix (gl-name field) i)))
                                 ((listp (gl-type field))
                                  (gather-fields (gl-struct (second (gl-type field)))
                                                 (format NIL "~a~a." prefix (gl-name field))))
                                 (T
                                  (list (format NIL "~a~a" prefix (gl-name field))))))))
      (let ((fields (gather-fields struct (format NIL "~@[~a.~]" (binding buffer)))))
        (cffi:with-foreign-objects ((names :pointer 1)
                                    (indices :int 1)
                                    (params :int 1))
          (dolist (field fields)
            (cffi:with-foreign-string (name field)
              (setf (cffi:mem-ref names :pointer) name)
              (%gl:get-uniform-indices (gl-name program) 1 names indices)
              (%gl:get-active-uniforms-iv (gl-name program) 1 indices :uniform-offset params)
              (setf (gethash field offsets) (cffi:mem-ref params :int)))))))
    (values offsets size)))

(defmethod load ((buffer uniform-buffer))
  ;; If the layout is std140, we can compute the size and offsets without a program.
  (when (find :std140 (enlist (layout buffer)))
    (multiple-value-bind (offsets size) (compute-offsets (input buffer) :std140)
      (setf (offsets buffer) offsets)
      (setf (size buffer) size)))
  (allocate buffer))

(defmethod bind ((buffer uniform-buffer) (program shader-program) (binding-point integer))
  ;; Calculate size and offsets now.
  (unless (offsets buffer)
    (multiple-value-bind (offsets size) (compute-offsets buffer program)
      (setf (offsets buffer) offsets)
      (setf (size buffer) size)))
  ;; FIXME: at this point we could compile an optimised accessor for the fields
  ;;        that has the offsets and such rolled out so that there's no lookup
  ;;        costs beyond calling the function from a slot.
  ;; Allocate the buffer with the correct sizing information.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (let ((index (gl:get-uniform-block-index (gl-name program) (gl-type buffer))))
    (%gl:uniform-block-binding (gl-name program) index binding-point)
    (%gl:bind-buffer-base :uniform-buffer binding-point (gl-name buffer))))


(defmethod buffer-field ((buffer uniform-buffer) field)
  (error "FIXME"))

(defmethod (setf buffer-field) (value (buffer uniform-buffer) field)
  ;; FIXME: wouldn't it be better to keep the C-memory block for the UBO around,
  ;;        write the values in there ourselves, and then call an update call
  ;;        instead of going through the slow, generic variants of buffer-object?
  (let ((offset (gethash field (offsets buffer))))
    ;; FIXME: Feature tag to remove this check
    (unless offset
      (error "Field ~s not found in ~a." field buffer))
    (update-buffer-data buffer value :buffer-start offset )))
