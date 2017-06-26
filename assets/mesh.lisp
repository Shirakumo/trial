#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass mesh (asset)
  ((mesh :initarg :mesh :accessor mesh)
   (size :initform 0 :accessor size))
  (:default-initargs :mesh NIL))

(defmethod coerce-input ((asset mesh) (input pathname))
  (read-geometry input T))

(defmethod coerce-input ((asset mesh) (input string))
  (read-geometry (pathname input) T))

(defmethod coerce-input ((asset mesh) (input geometry))
  input)

(defmethod coerce-input ((asset mesh) (input vertex-mesh))
  input)

(defmethod coerce-input ((asset mesh) (input vertex-array))
  input)

(defmethod finalize-resource ((type (eql 'mesh)) resource)
  (finalize-resource 'vertex-array resource))

(defmethod load progn ((asset mesh))
  (let* ((geometry (first (coerced-inputs asset)))
         (mesh (etypecase geometry
                 (geometry (or (gethash (mesh asset) (meshes geometry))
                               (error "~a does not contain a mesh named ~a."
                                      geometry (mesh asset))))
                 (T geometry))))
    (etypecase mesh
      (vertex-mesh
       (let ((new (make-instance 'vertex-mesh :face-length (face-length mesh))))
         (setf (faces new) (faces mesh))
         (setf (vertices new) (vertices mesh))
         (change-class new 'vertex-array :load T)
         (setf (resource asset) (resource new))
         (setf (size asset) (size new))))
      (vertex-array
       (setf (resource asset) (resource (load mesh)))
       (setf (size asset) (size mesh))))))
