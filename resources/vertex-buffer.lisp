(in-package #:org.shirakumo.fraf.trial)

(defmethod element-type ((array array))
  (cl-type->gl-type (array-element-type array)))

(defun ensure-element-type (element-type array)
  (if (eql element-type (cl-type->gl-type (array-element-type array)))
      array
      (let ((res (make-array (length array) :element-type (gl-type->cl-type element-type))))
        #-trial-release (v:warn :trial "Coercing array of type ~a to match element type ~a"
                                (cl-type->gl-type (array-element-type array)) element-type)
        (map-into res (gl-coercion-fun element-type) array))))

(defclass vertex-buffer (bindable-buffer)
  ((element-type :initarg :element-type :accessor element-type)
   (gl-type :initarg :gl-type :initform NIL :accessor gl-type)
   (binding :initform NIL))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float))

(defmethod initialize-instance :before ((buffer vertex-buffer) &key element-type)
  (check-vertex-buffer-element-type element-type))

(defmethod initialize-instance :after ((buffer vertex-buffer) &key)
  (unless (gl-type buffer)
    (setf (gl-type buffer) (ecase (buffer-type buffer)
                             (:element-array-buffer "IndexData")
                             (:array-buffer "VertexData")))))

(defmethod shared-initialize :after ((buffer vertex-buffer) slots &key count)
  (when count (setf (size buffer) (* (gl-type-size (element-type buffer)) count))))

(defmethod allocate :before ((buffer vertex-buffer))
  (let ((buffer-data (buffer-data buffer)))
    (when (and (not (size buffer)) (vectorp buffer-data))
      (setf (size buffer) (* (length buffer-data) (gl-type-size (element-type buffer)))))))

(defmethod binding-target ((buffer vertex-buffer)) :shader-storage-buffer)
(defmethod layout-standard ((buffer vertex-buffer)) 'std430)

(defmethod struct-fields ((buffer vertex-buffer))
  `((glsl-toolkit:struct-declarator (glsl-toolkit:type-qualifier)
                                    (glsl-toolkit:type-specifier ,(ecase (element-type buffer)
                                                                    (:unsigned-int :uint)
                                                                    (:int :int)
                                                                    (:float :float)
                                                                    (:double :double)))
                                    ,(cffi:translate-underscore-separated-name
                                      (cffi:translate-camelcase-name (gl-type buffer)))
                                    (glsl-toolkit:array-specifier))))

(defmethod bind ((buffer vertex-buffer) (program shader-program))
  (load buffer)
  (%gl:shader-storage-block-binding
   (gl-name program)
   (cffi:with-foreign-string (var (gl-type buffer))
     (%gl:get-program-resource-index (gl-name program) :shader-storage-block var))
   (or (binding-point buffer)
       (setf (binding-point buffer) T))))

(defmethod update-buffer-data ((buffer vertex-buffer) (data vector) &rest args)
  (apply #'call-next-method buffer (ensure-element-type (element-type buffer) data) args))

(defmethod resize-buffer-data ((buffer vertex-buffer) (data vector) &key (data-start 0) size ((:data _)))
  (declare (ignore _))
  (mem:with-memory-region (region (ensure-element-type (element-type buffer) data) :offset data-start)
    #-elide-buffer-access-checks
    (when (and size (not (cffi:null-pointer-p (memory-region-pointer region))) (< (memory-region-size region) size))
      (error "Attempting to update ~d bytes from ~a, when it has only ~d bytes available."
             size data (memory-region-size region)))
    (resize-buffer-data/ptr buffer (or size (memory-region-size region)) (memory-region-pointer region))))
