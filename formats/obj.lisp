(defpackage #:org.shirakumo.fraf.trial.obj
  (:use #:cl+trial)
  (:shadow #:asset)
  (:local-nicknames
   (#:obj #:org.shirakumo.fraf.wavefront))
  (:export))
(in-package #:org.shirakumo.fraf.trial.obj)

(defun generate-image (asset texture-map)
  (when texture-map
    (generate-resources 'image-loader (obj:file texture-map)
                        :resource (resource asset (obj:file texture-map))
                        :mag-filter (if (obj:blend-u texture-map) :linear :nearest)
                        :min-filter (if (obj:blend-u texture-map) :linear :nearest)
                        :wrapping (list (if (obj:clamp texture-map) :clamp-to-edge :repeat)
                                        (if (obj:clamp texture-map) :clamp-to-edge :repeat)
                                        (if (obj:clamp texture-map) :clamp-to-edge :repeat)))))

(defun to-vec (a &optional (arity (length a)))
  (flet ((ref (x)
           (if (< x (length a)) (aref a x) 0f0)))
    (ecase arity
      (2 (vec (ref 0) (ref 1)))
      (3 (vec (ref 0) (ref 1) (ref 2)))
      (4 (vec (ref 0) (ref 1) (ref 2) (ref 3))))))

(defmethod load-model (input (type (eql :obj)) &key (generator (make-instance 'resource-generator))
                                                    (model (make-instance 'trial:model)))
  (let ((context (obj:parse input)))
    (loop for material being the hash-values of (obj:materials context)
          do (setf (trial:find-material (obj:name material) model)
                   (if (or (obj:metallic-factor material) (obj:roughness-factor material)
                           (obj:metallic-map material) (obj:roughness-map material)
                           (obj:rough-metal-occlusion-map material))
                       (trial:ensure-instance
                        (trial:find-material (obj:name material) model NIL) 'trial:pbr-material
                        :albedo-texture (generate-image generator (obj:diffuse-map material))
                        :metal-rough-texture (generate-image generator (obj:rough-metal-occlusion-map material))
                        :metallic-texture (generate-image generator (obj:metallic-map material))
                        :roughness-texture (generate-image generator (obj:roughness-map material))
                        :emissive-texture (generate-image generator (obj:emissive-map material))
                        :normal-texture (generate-image generator (obj:normal-map material))
                        :albedo-factor (to-vec (obj:diffuse-factor material) 4)
                        :metallic-factor (obj:metallic-factor material)
                        :roughness-factor (obj:roughness-factor material)
                        :emissive-factor (to-vec (obj:emissive-factor material)))
                       (trial:update-material
                        (trial:find-material (obj:name material) model NIL) 'trial::phong-material
                        :diffuse-texture (generate-image generator (obj:diffuse-map material))
                        :specular-texture (generate-image generator (obj:specular-map material))
                        :normal-texture (generate-image generator (obj:normal-map material))
                        :diffuse-factor (to-vec (obj:diffuse-factor material))
                        :specular-factor (obj:specular-factor material)))))
    (flet ((add-mesh (name mesh)
             (setf (trial:find-mesh name model)
                   (make-instance 'trial:static-mesh
                                  :name name
                                  :vertex-attributes (loop for attribute in (obj:attributes mesh)
                                                           collect (ecase attribute
                                                                     (:position 'location)
                                                                     (:normal 'normal)
                                                                     (:uv 'uv)))
                                  :vertex-data (obj:vertex-data mesh)
                                  :index-data (obj:index-data mesh)
                                  :material (when (obj:material mesh)
                                              (trial:find-material (obj:name (obj:material mesh)) model))))))
      (loop for object being the hash-values of (obj:objects context)
            for meshes = (obj:extract-meshes context object)
            do (if (rest meshes)
                   (loop for mesh in meshes
                         for i from 0
                         do (add-mesh (cons (obj:name object) i) mesh))
                   (add-mesh (obj:name object) (first meshes))))
      model)))
