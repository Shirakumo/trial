#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-struct-buffer (struct-buffer vertex-buffer)
  ((struct-count :initarg :struct-count :initform (error "STRUCT-COUNT required.") :accessor struct-count)
   (struct-vector :accessor struct-vector)))

(defmethod reinitialize-instance :after ((buffer vertex-struct-buffer) &key struct-count)
  (when struct-count (c2mop:update-dependent (struct-class buffer) buffer)))

(defmethod vertex-layout ((buffer vertex-struct-buffer))
  (vertex-layout (struct-class buffer)))

(defmethod add-vertex-bindings ((vbo vertex-struct-buffer) (vao vertex-array))
  (let ((idx (loop for i from 0
                   for binding in (bindings vao)
                   when (listp binding)
                   maximize (getf (rest binding) :index i))))
    (loop for i from (1+ idx)
          for binding in (vertex-layout vbo)
          do (push (list* vbo :index i :instancing 1 binding) (bindings vao)))
    (values vao (+ 1 idx))))

(defmethod buffer-field-size ((buffer vertex-struct-buffer) standard base)
  (* (call-next-method)
     (struct-count buffer)))

(defmethod (setf struct-count) :after (value (buffer vertex-struct-buffer))
  (c2mop:update-dependent (struct-class buffer) buffer))

(defmethod (setf buffer-data) :after (data (buffer vertex-struct-buffer))
  (loop with struct = (struct-class buffer)
        with vector = (make-array (struct-count buffer))
        with size = (buffer-field-stride buffer struct)
        for i from 0 below (length vector)
        for offset = 0 then (+ offset size)
        do (setf (aref vector i) (make-instance struct
                                                :storage (mem:to-memory-region data)
                                                :base-offset offset))
        finally (setf (struct-vector buffer) vector)))

(defmethod deallocate :after ((buffer vertex-struct-buffer))
  (slot-makunbound buffer 'struct-vector))
