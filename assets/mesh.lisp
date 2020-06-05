#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass mesh-loader (resource-generator)
  ())

(defmethod generate-resources ((generator mesh-loader) (mesh vertex-mesh) &key (data-usage :static-draw) (vertex-attributes T) vertex-form (resource (resource generator T)))
  (let* ((vao (ensure-instance resource 'vertex-array
                               :vertex-form (or vertex-form
                                                (ecase (face-length mesh)
                                                  (1 :points)
                                                  (2 :lines)
                                                  (3 :triangles)))))
         (primer (if (= 0 (length (vertices mesh)))
                     (allocate-instance (find-class (vertex-type mesh)))
                     (aref (vertices mesh) 0)))
         (attributes (etypecase vertex-attributes
                       ((eql T) (vertex-attributes primer))
                       (list vertex-attributes)))
         (sizes (loop for attr in attributes collect (vertex-attribute-size primer attr)))
         (buffer (make-vertex-data mesh :attributes attributes))
         ;; FIXME: The assumption of float-only packing here is too primitive.
         ;;        The same problem exists in geometry.lisp, though.
         (vbo (make-instance 'vertex-buffer :buffer-data buffer :buffer-type :array-buffer
                                            :data-usage data-usage :element-type :float
                                            :size (* (length buffer) (gl-type-size :float))))
         (ebo (make-instance 'vertex-buffer :buffer-data (faces mesh) :buffer-type :element-array-buffer
                                            :data-usage data-usage :element-type :unsigned-int
                                            :size (* (length (faces mesh)) (gl-type-size :unsigned-int))))
         (specs (loop with stride = (reduce #'+ sizes)
                      for offset = 0 then (+ offset size)
                      for size in sizes
                      for index from 0
                      collect (list vbo :stride (* stride (gl-type-size :float))
                                        :offset (* offset (gl-type-size :float))
                                        :size size
                                        :index index))))
    (setf (bindings vao) (list* ebo specs))
    vao))

(defmethod generate-resources ((generator mesh-loader) (path pathname) &key (format T))
  (let ((meshes (meshes (read-geometry path format))))
    (loop for name being the hash-keys of meshes
          for mesh being the hash-values of meshes
          collect (generate-resources generator mesh :resource (resource generator name)))))

(defclass mesh (multi-resource-asset mesh-loader)
  ())
