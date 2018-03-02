#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass mesh (gl-asset vertex-array)
  ((geometry-name :initarg :geometry-name :accessor geometry-name)
   (attributes :initarg :attributes :accessor attributes)
   (data-usage :initarg :data-usage :accessor data-usage))
  (:default-initargs
   :geometry-name NIL
   :data-usage :static-draw
   :attributes T))

(defmethod load ((mesh mesh))
  (let* ((input (coerce-asset-input mesh T))
         (mesh (etypecase input
                 (pathname (gethash (geometry-name mesh) (meshes (read-geometry input T))))
                 (geometry (gethash (geometry-name mesh) (meshes input)))
                 (vertex-mesh input))))
    (let* ((vertices (vertices mesh))
           (primer (aref vertices 0))
           (attributes (etypecase (attributes mesh)
                         ((eql T) (vertex-attributes primer))
                         (list (attributes mesh))))
           (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
           (total-size (* (length vertices) (reduce #'+ sizes)))
           (buffer (make-static-vector total-size :element-type 'single-float)))
      ;; Copy the contents of the mesh into the data buffer, packed.
      (loop with buffer-offset = 0
            for vertex across vertices
            do (dolist (attribute attributes)
                 (setf buffer-offset (fill-vertex-attribute vertex attribute buffer buffer-offset))))
      ;; Construct the buffers and specs
      (let* ((vbo (make-instance 'vertex-buffer :buffer-data buffer :buffer-type :array-buffer
                                                :data-usage (data-usage mesh) :element-type :float))
             (ebo (make-instance 'vertex-buffer :buffer-data (faces mesh) :buffer-type :element-array-buffer
                                                :data-usage (data-usage mesh) :element-type :uint))
             (specs (loop with stride = (reduce #'+ sizes)
                          for offset = 0 then (+ offset size)
                          for size in sizes
                          for index from 0
                          collect (list vbo :stride (* stride (cffi:foreign-type-size :float))
                                            :offset (* offset (cffi:foreign-type-size :float))
                                            :size size
                                            :index index))))
        (setf (buffers mesh) (list* ebo specs))
        (setf (size mesh) (length vertices))
        (allocate mesh)
        ;; Clean up
        (deallocate vbo)
        (deallocate ebo)
        (setf (buffer-data vbo) NIL)
        (setf (buffer-data ebo) NIL)
        (setf (buffers mesh) NIL)
        (static-vectors:free-static-vector buffer)))))
