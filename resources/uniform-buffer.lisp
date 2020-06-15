#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-buffer (struct-buffer)
  ((buffer-type :initform :uniform-buffer)
   (qualifiers :initarg :qualifiers :initform () :accessor qualifiers)
   (binding :initarg :binding :initform NIL :accessor binding)))

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

(defmethod bind ((buffer uniform-buffer) (program shader-program) (binding-point integer))
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (let ((index (gl:get-uniform-block-index (gl-name program) (gl-type buffer))))
    (%gl:uniform-block-binding (gl-name program) index binding-point)
    (%gl:bind-buffer-base :uniform-buffer binding-point (gl-name buffer))))

