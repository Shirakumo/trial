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

(defun from-vec (v &optional (arity (length (varr v))))
  (flet ((ref (x)
           (if (< x (length (varr v))) (aref (varr v) x) 0f0)))
    (ecase arity
      (2 (vector (ref 0) (ref 1)))
      (3 (vector (ref 0) (ref 1) (ref 2)))
      (4 (vector (ref 0) (ref 1) (ref 2) (ref 3))))))

(defun to-meshes (meshes name)
  (flet ((make-mesh (mesh name)
           (let ((attributes (loop for attribute in (vertex-attributes mesh)
                                   when (find attribute '(location uv uv-0 normal))
                                   collect attribute)))
             (make-instance 'obj:mesh :name name
                                      :material (name (material mesh))
                                      :attributes (loop for attribute in attributes
                                                        collect (case attribute
                                                                  (location :position)
                                                                  ((uv uv-0) :uv)
                                                                  (normal :normal)))
                                      :vertex-data (reordered-vertex-data mesh attributes)
                                      :index-data (faces mesh)))))
    (cond ((rest meshes)
           (loop for i from 0
                 for mesh in meshes
                 collect (make-mesh mesh (format NIL "~a-~d" name i))))
          (T
           (list (make-mesh (first meshes) name))))))

(defmethod load-model ((input stream) (type (eql :obj)) &key (generator (make-instance 'resource-generator))
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
                                  :faces (obj:index-data mesh)
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

(defmethod save-model ((model model) (target stream) (type (eql :obj)) &key (if-exists :error) (export-materials T) (material-library-file :create) (texture-format "png"))
  (let ((context (make-instance 'obj:context)))
    (when export-materials
      (loop for name being the hash-keys of (materials model) using (hash-value material)
            do (setf (gethash name (obj:materials context))
                     (flet ((from-texture (type texture)
                              (let ((name (format NIL "~a-~a" name type)))
                                (save-image texture (make-pathname :name name :type texture-format :defaults target) T))))
                       (etypecase material
                         (pbr-material
                          (make-instance 'obj:material
                                         :diffuse-map (from-texture "diffuse" (albedo-texture material))
                                         ;; FIXME: reorder?
                                         :rough-metal-occlusion-map (from-texture "metal-rough-occlusion" (metal-rough-occlusion-texture material))
                                         :emissive-map (from-texture "emissive" (emission-texture material))
                                         :normal-map (from-texture "normal" (normal-texture material))
                                         :diffuse-factor (from-vec (albedo-factor material) 3)
                                         :metallic-factor (metalness-factor material)
                                         :roughness-factor (roughness-factor material)
                                         :emissive-factor (from-vec (emission-factor material))))
                         (phong-material
                          (make-instance 'obj:material
                                         :diffuse-map (from-texture "diffuse" (diffuse-texture material))
                                         :specular-map (from-texture "specular" (specular-texture material))
                                         :normal-map (from-texture "normal" (normal-texture material))
                                         :diffuse-factor (from-vec (diffuse-factor material) 3)
                                         :specular-factor (specular-factor material))))))))
    (let ((meshes (make-hash-table :test 'equal)))
      (loop for name being the hash-keys of (meshes model) using (hash-value mesh)
            do (if (listp name)
                   (destructuring-bind (name . i) name
                     (push (cons mesh i) (gethash name meshes)))
                   (push (cons mesh 0) (gethash name meshes))))
      (loop for name being the hash-keys of meshes using (hash-value list)
            for meshlist = (mapcar #'car (sort list #'< :key #'cdr))
            for objlist = (to-meshes meshlist name)
            do (obj:combine-meshes objlist context)
               (let ((groups (make-hash-table :test 'equal)))
                 (loop for obj in objlist
                       do (setf (gethash (obj:name obj) groups) (gethash (obj:name obj) (obj:groups context))))
                 (setf (gethash name (obj:objects context))
                       (make-instance 'obj:object :name name :group groups)))))
    (obj:serialize context target :if-exists if-exists :export-materials export-materials :material-library-file material-library-file)))
