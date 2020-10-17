#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *uniform-binding-point-counter* 0)

(defclass uniform-buffer (struct-buffer)
  ((buffer-type :initform :uniform-buffer)
   (qualifiers :initarg :qualifiers :initform () :accessor qualifiers)
   (binding :initarg :binding :initform NIL :accessor binding)
   (binding-point :initarg :binding-point :initform NIL :accessor binding-point)))

(defmethod shared-initialize :after ((buffer uniform-buffer) slots &key struct-class binding)
  (when (or binding (null (binding buffer)))
    (setf (binding buffer) (cffi:translate-underscore-separated-name
                            (class-name (ensure-class struct-class))))))

(defmethod gl-source ((buffer uniform-buffer))
  `(glsl-toolkit:shader
    ,@(loop for dependent in (compute-dependent-types buffer)
            collect (gl-source (find-class dependent)))
    (glsl-toolkit:interface-declaration
     (glsl-toolkit:type-qualifier
      (glsl-toolkit:layout-qualifier
       (glsl-toolkit:layout-qualifier-id ,(layout-standard (struct-class buffer))))
      :uniform
      ,@(qualifiers buffer))
     ,(gl-type buffer)
     ,(if (binding buffer)
          `(glsl-toolkit:instance-name ,(binding buffer))
          'glsl-toolkit:no-value)
     ,@(mapcar #'gl-source (struct-fields buffer)))))

(defmethod allocate :after ((buffer uniform-buffer))
  (unless (binding-point buffer)
    (setf (binding-point buffer) (incf *uniform-binding-point-counter*)))
  (%gl:bind-buffer-base :uniform-buffer (binding-point buffer) (gl-name buffer)))

(defmethod bind ((buffer uniform-buffer) (program shader-program))
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (%gl:uniform-block-binding
   (gl-name program)
   (gl:get-uniform-block-index (gl-name program) (gl-type buffer))
   (binding-point buffer)))
