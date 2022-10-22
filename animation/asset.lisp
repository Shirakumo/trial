#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass animation-asset (asset)
  ((meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (clips :initform (make-hash-table :test 'equal) :accessor clips)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod generate-resources :around ((asset animation-asset) input &key)
  (let ((meshes (meshes asset))
        (clips (clips asset)))
    (clrhash meshes)
    (clrhash clips)
    (setf (skeleton asset) NIL)
    (call-next-method)
    (loop for mesh being the hash-values of meshes
          collect (resource asset (name mesh)))))
