#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass bindable-buffer (buffer-object)
  ((binding-point :initarg :binding-point :initform NIL :accessor binding-point)
   (qualifiers :initarg :qualifiers :initform NIL :accessor qualifiers)
   (binding :initarg :binding :accessor binding)))

(defmethod shared-initialize :after ((buffer bindable-buffer) slots &key (binding NIL binding-p))
  (when binding-p (setf (binding buffer) binding)))

(defmethod compute-dependent-types ((buffer bindable-buffer)))
(defgeneric binding-target (buffer-object))
(defgeneric bind (buffer-object program))

(defmethod gl-source ((buffer bindable-buffer))
  `(glsl-toolkit:shader
    ,@(loop for dependent in (compute-dependent-types buffer)
            collect (gl-source (find-class dependent)))
    (glsl-toolkit:interface-declaration
     (glsl-toolkit:type-qualifier
      (glsl-toolkit:layout-qualifier
       (glsl-toolkit:layout-qualifier-id ,(intern (string (layout-standard buffer)) "KEYWORD"))
       (glsl-toolkit:layout-qualifier-id "row_major"))
      ;; KLUDGE: this is kinda dirty.
      ,(ecase (binding-target buffer)
         (:shader-storage-buffer :buffer)
         (:uniform-buffer :uniform))
      ,@(qualifiers buffer))
     ,(gl-type buffer)
     ,(if (binding buffer)
          `(glsl-toolkit:instance-name ,(binding buffer))
          'glsl-toolkit:no-value)
     ,@(struct-fields buffer))))

(defmethod (setf binding-point) :after ((point integer) (buffer bindable-buffer))
  (when (and (allocated-p buffer) (integerp (binding-point buffer)))
    (%gl:bind-buffer-base (binding-target buffer) (binding-point buffer) (gl-name buffer))))

(defmethod (setf binding-point) :before ((point null) (buffer bindable-buffer))
  (when (and (allocated-p buffer) (integerp (binding-point buffer)))
    (%gl:bind-buffer-base (binding-target buffer) (binding-point buffer) 0)))

(defmethod (setf binding-point) ((point (eql T)) (buffer bindable-buffer))
  (setf (binding-point buffer) NIL)
  (setf (binding-point buffer)
        ;; FIXME: this scan could be done faster, no?
        (loop with allocator = (binding-point-allocator *context*)
              for i from 0 below (length allocator)
              do (when (= 0 (sbit allocator i))
                   (setf (sbit allocator i) 1)
                   (return i))
              finally (error "What the heck?? Out of buffer binding points"))))

(defmethod allocate :after ((buffer bindable-buffer))
  (when (binding-point buffer)
    (setf (binding-point buffer) (binding-point buffer))))

(defmethod deallocate :after ((buffer bindable-buffer))
  (when (binding-point buffer)
    (setf (sbit (binding-point-allocator *context*) (binding-point buffer)) 0)
    (setf (binding-point buffer) NIL)))
