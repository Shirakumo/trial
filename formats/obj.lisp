#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.obj
  (:use #:cl+trial)
  (:shadow #:asset)
  (:local-nicknames
   (#:obj #:org.shirakumo.fraf.wavefront))
  (:export
   #:asset))
(in-package #:org.shirakumo.fraf.trial.obj)

(defclass asset (file-input-asset
                 multi-resource-asset
                 animation-asset
                 trial::full-load-asset)
  ())

(defun generate-image (asset texture-map)
  (when texture-map
    (generate-resources 'image-loader (obj:file texture-map)
                        :resource (resource asset (obj:file texture-map))
                        :mag-filter (if (obj:blend-u texture-map) :linear :nearest)
                        :min-filter (if (obj:blend-u texture-map) :linear :nearest)
                        :wrapping (list (if (obj:clamp texture-map) :clamp-to-edge :repeat)
                                        (if (obj:clamp texture-map) :clamp-to-edge :repeat)
                                        (if (obj:clamp texture-map) :clamp-to-edge :repeat)))))

(defmethod generate-resources ((asset asset) input &key)
  (let* ((context (obj:parse input)))
    (let ((mesh-table (meshes asset)))
      (loop for object being the hash-values of (obj:objects context)
            ;; KLUDGE: we currently only allow one material per object.
            for (mesh) = (obj:extract-meshes context object)
            for material = (obj:material mesh)
            do (setf (gethash (obj:name object) mesh-table)
                     (make-instance 'trial:static-mesh 
                                    :name (obj:name object)
                                    :vertex-data (obj:vertex-data mesh)
                                    :index-data (obj:index-data mesh)))
               (if (or (obj:metallic-factor material) (obj:roughness-factor material)
                       (obj:metallic-map material) (obj:roughness-map material)
                       (obj:rough-metal-occlusion-map material))
                   (trial:update-material
                    (obj:name material) 'trial:pbr-material
                    :albedo-texture (generate-image asset (obj:diffuse-map material))
                    :metal-rough-texture (generate-image asset (obj:rough-metal-occlusion-map material))
                    :metallic-texture (generate-image asset (obj:metallic-map material))
                    :roughness-texture (generate-image asset (obj:roughness-map material))
                    :emissive-texture (generate-image asset (obj:emissive-map material))
                    :normal-texture (generate-image asset (obj:normal-map material))
                    :albedo-factor (to-vec (obj:diffuse-factor material))
                    :metallic-factor (obj:metallic-factor material)
                    :roughness-factor (obj:roughness-factor material)
                    :emissive-factor (to-vec (obj:emissive-factor material)))
                   (trial:update-material
                    (obj:name material) 'trial::phong-material
                    :diffuse-texture (generate-image asset (obj:diffuse-map material))
                    :specular-texture (generate-image asset (obj:specular-map material))
                    :normal-texture (generate-image asset (obj:normal-map material))
                    :diffuse-factor (to-vec (obj:diffuse-factor material))
                    :specular-factor (obj:specular-factor)))))))
