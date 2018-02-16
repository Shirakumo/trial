#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass mesh (asset vertex-array)
  ((mesh :initarg :mesh :accessor mesh)
   (size :initform 0 :accessor size))
  (:default-initargs :mesh NIL))

(defmethod load ((mesh mesh))
  (let* ((geometry (first (coerced-inputs mesh)))
         (mesh (etypecase geometry
                 (geometry (or (gethash (mesh mesh) (meshes geometry))
                               (error "~a does not contain a mesh named ~a."
                                      geometry (mesh mesh))))
                 (T geometry))))
    (etypecase mesh
      (vertex-mesh
       (let ((new (make-instance 'vertex-mesh :face-length (face-length mesh))))
         (setf (faces new) (faces mesh))
         (setf (vertices new) (vertices mesh))
         (change-class new 'vertex-array :load T)
         (setf (resource mesh) (resource new))
         (setf (size mesh) (size new))))
      (vertex-array
       (setf (resource mesh) (resource (load mesh)))
       (setf (size mesh) (size mesh))))))

(defmethod update-instance-for-different-class :after ((mesh vertex-mesh) (vao vertex-array) &key pack load (data-usage :static-draw) attributes)
  (when pack (pack mesh))
  (let* ((vertices (vertices mesh))
         (primer (aref vertices 0))
         (attributes (or attributes (vertex-attributes primer)))
         (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
         (total-size (* (length vertices) (reduce #'+ sizes)))
         (buffer (make-static-vector total-size :element-type 'single-float)))
    (loop with offset = 0
          for vertex across vertices
          do (dolist (attribute attributes)
               (setf offset (fill-vertex-attribute vertex attribute buffer offset))))
    (let* ((vbo (make-asset 'vertex-buffer buffer
                            :data-usage data-usage :element-type :float :buffer-type :array-buffer))
           (ebo (make-asset 'vertex-buffer (faces mesh)
                            :data-usage data-usage :element-type :uint :buffer-type :element-array-buffer))
           (specs (loop with stride = (reduce #'+ sizes)
                        for offset = 0 then (+ offset size)
                        for size in sizes
                        for index from 0
                        collect (list vbo :stride (* stride (cffi:foreign-type-size :float))
                                          :offset (* offset (cffi:foreign-type-size :float))
                                          :size size
                                          :index index))))
      (setf (inputs vao) (list* ebo specs))
      (when load
        (load vao)
        ;; Clean up
        (offload vbo)
        (offload ebo)
        (setf (inputs vbo) NIL)
        (setf (inputs ebo) NIL)
        (setf (inputs vao) NIL)
        (static-vectors:free-static-vector buffer))
      vao)))
