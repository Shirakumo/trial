#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(defclass gltf-asset (trial:file-input-asset
                      trial:multi-resource-asset
                      trial::full-load-asset)
  ((meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (skeleton :initform NIL :accessor skeleton)
   (clips :initform (make-hash-table) :accessor clips)))

(defmethod trial:generate-resources ((asset gltf-asset) input &key)
  (gltf:with-gltf (gltf input)
    (let ((meshes (meshes asset)))
      (clrhash meshes)
      (clrhash (clips asset))
      (setf (skeleton asset) NIL)
      (loop for mesh across (load-meshes gltf)
            for i from 0
            do (unless (trial:name mesh)
                 (setf (trial:name mesh) i))
               (setf (gethash (trial:name mesh) meshes) mesh)
               (when (trial:texture mesh)
                 (setf (trial:texture mesh)
                       NIL #++(trial:resource asset (list 'texture (gltf:idx (trial:texture mesh))))))
               (make-vertex-array mesh (trial:resource asset (trial:name mesh))))
      #++
      (loop for material across (gltf:materials gltf)
            do (let* ((pbr (gltf:pbr material))
                      (texinfo (gltf:albedo pbr))
                      (texture (gltf:texture texinfo))
                      (sampler (gltf:sampler texture))
                      (image (gltf:source texture))
                      (name (list 'texture (gltf:idx material))))
                 (trial:generate-resources 'trial:image-loader (gltf:path image)
                                           :resource (trial:resource asset name)
                                           :mag-filter (gltf:mag-filter sampler)
                                           :min-filter (gltf:min-filter sampler)
                                           :wrapping (list (gltf:wrap-s sampler)
                                                           (gltf:wrap-t sampler)
                                                           :clamp-to-edge))))
      (when (loop for mesh being the hash-values of meshes
                  thereis (skinned-p mesh))
        (setf (skeleton asset) (load-skeleton gltf))
        (setf (clips asset) (load-clips gltf))
        (let ((map (make-hash-table :test 'eql)))
          (reorder (skeleton asset) map)
          (loop for clip being the hash-values of (clips asset)
                do (reorder clip map))
          (loop for mesh being the hash-values of (meshes asset)
                do (reorder mesh map))))
      (print (list :meshes (alexandria:hash-table-keys (meshes asset))))
      (print (list :clips (alexandria:hash-table-keys (clips asset))))
      (loop for mesh being the hash-values of meshes
            collect (trial:resource asset (trial:name mesh))))))
