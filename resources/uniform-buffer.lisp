#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-buffer (struct-buffer)
  ((buffer-type :initform :uniform-buffer)
   (qualifiers :initarg :qualifiers :initform () :accessor qualifiers)
   (binding :initarg :binding :accessor binding)
   (binding-point :initarg :binding-point :initform NIL :accessor binding-point)))

(defmethod shared-initialize :after ((buffer uniform-buffer) slots &key (binding NIL binding-p))
  (cond (binding-p
         (setf (binding buffer) binding))
        ((not (slot-boundp buffer 'binding))
         (setf (binding buffer) (cffi:translate-underscore-separated-name
                                 (class-name (class-of (struct buffer))))))))

(defmethod gl-source ((buffer uniform-buffer))
  `(glsl-toolkit:shader
    ,@(loop for dependent in (compute-dependent-types buffer)
            collect (gl-source (find-class dependent)))
    (glsl-toolkit:interface-declaration
     (glsl-toolkit:type-qualifier
      (glsl-toolkit:layout-qualifier
       (glsl-toolkit:layout-qualifier-id ,(intern (string (layout-standard buffer)) "KEYWORD")))
      :uniform
      ,@(qualifiers buffer))
     ,(gl-type buffer)
     ,(if (binding buffer)
          `(glsl-toolkit:instance-name ,(binding buffer))
          'glsl-toolkit:no-value)
     ,@(let ((*dynamic-context* (struct buffer)))
         (mapcar #'gl-source (struct-fields buffer))))))

(defmethod allocate :after ((buffer uniform-buffer))
  (unless (binding-point buffer)
    (setf (binding-point buffer)
          (loop with allocator = (binding-point-allocator *context*)
                for i from 0 below (length allocator)
                do (when (= 0 (sbit allocator i))
                     (setf (sbit allocator i) 1)
                     (return i))
                finally (error "What the heck?? Out of buffer binding points"))))
  (%gl:bind-buffer-base (buffer-type buffer) (binding-point buffer) (gl-name buffer)))

(defmethod deallocate :after ((buffer uniform-buffer))
  (setf (sbit (binding-point-allocator *context*) (binding-point buffer)) 0)
  (setf (binding-point buffer) NIL))

(defmethod bind ((buffer uniform-buffer) (program shader-program))
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (let ((index (gl:get-uniform-block-index (gl-name program) (gl-type buffer))))
    (if (= (1- (ash 1 32)) index)
        (warn "Failed to get uniform block index for ~s in ~a"
              (gl-type buffer) program)
        (%gl:uniform-block-binding (gl-name program) index (binding-point buffer)))))
