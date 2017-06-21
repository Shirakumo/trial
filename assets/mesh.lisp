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
         (own (eql geometry (first (inputs asset))))
         (mesh (etypecase geometry
                 (geometry (or (gethash (mesh asset) (meshes geometry))
                               (error "~a does not contain a mesh named ~a."
                                      geometry (mesh asset))))
                 (T geometry))))
    (etypecase mesh
      (vertex-mesh
       (unless own
         (let ((new (make-instance 'vertex-mesh :face-length (face-length mesh))))
           (setf (faces new) (faces mesh))
           (setf (vertices new) (vertices mesh))
           (setf mesh new)))
       (change-class mesh 'vertex-array :pack T :load T)
       (setf (resource asset) (resource mesh)))
      (vertex-array
       (setf (resource asset) (resource (load mesh)))
       (when own
         (loop for (buffer) in (inputs mesh)
               do (offload buffer)
                  (maybe-free-static-vector (unlist (inputs buffer)))
                  (setf (inputs buffer) NIL)))))
    (setf (size asset) (size mesh))))
