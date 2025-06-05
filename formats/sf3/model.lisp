(in-package #:org.shirakumo.fraf.trial)

(defun %decode-sf3-vertex-attribute (attribute)
  (ecase attribute
    (:position 'location)
    (:uv 'uv)
    (:normal 'normal)
    (:color 'color)
    (:tangent 'tangent)))

(defmethod load-model (input (type (eql :sf3)) &key (generator (make-instance 'resource-generator))
                                                    (model (make-instance 'trial:model)))
  (let ((sf3 (org.shirakumo.sf3:read-sf3 input)))
    (check-type sf3 org.shirakumo.sf3:model)
    (when (org.shirakumo.sf3:texture-types sf3)
      (setf (find-material T model)
            (apply #'ensure-instance (find-material T model NIL) 'pbr-material
                   (loop for path across (org.shirakumo.sf3:textures sf3)
                         for type in (org.shirakumo.sf3:texture-types sf3)
                         collect (ecase type
                                   (:albedo :albedo-texture)
                                   (:normal :normal-texture)
                                   (:metallic :metallic-texture)
                                   (:metalness :metalness-textur)
                                   (:roughness :roughness-texture)
                                   (:occlusion :occlusion-texture)
                                   (:emission :emissive-texture)
                                   (:specular :specular-texture))
                         collect (generate-resources 'image-loader path
                                                     :resource (resource generator path))))))
    (setf (trial:find-mesh T model)
          (make-instance 'trial:static-mesh
                         :name T
                         :vertex-attributes (mapcar #'%decode-sf3-vertex-attribute
                                                    (org.shirakumo.sf3:vertex-attributes sf3))
                         :vertex-data (org.shirakumo.sf3:vertices sf3)
                         :faces (org.shirakumo.sf3:faces sf3)
                         :material (trial:find-material T model NIL)))))

(defmethod save-model ((mesh mesh-data) target (type (eql :sf3)) &key (if-exists :error) (export-textures T) (texture-format "png"))
  (let ((attributes (loop for attr in (vertex-attributes mesh)
                          for native = (case attr
                                         (location :position)
                                         (normal :normal)
                                         ((uv uv-0) :uv)
                                         ((color color-0) :color)
                                         ((tangent tangent-0) :tangent))
                          collect native))
        (material (material mesh))
        (textures ()))
    (flet ((from-texture (type texture)
             (when texture
               (let* ((name (format NIL "~a-~a" (name mesh) (string-downcase type)))
                      (path (make-pathname :name name :type texture-format :defaults target)))
                 (setf (getf textures type) (namestring path))
                 (when export-textures
                   (save-image texture path T))))))
      (etypecase material
        (null)
        (pbr-material
         (from-texture :albedo (albedo-texture material))
         (from-texture :metallic (metal-rough-occlusion-texture material))
         (from-texture :emissive (emission-texture material))
         (from-texture :normal (normal-texture material)))
        (phong-material
         (from-texture :albedo (diffuse-texture material))
         (from-texture :specular (specular-texture material))
         (from-texture :normal (normal-texture material)))))
    (org.shirakumo.sf3:write-sf3
     (org.shirakumo.sf3:make-model
      (faces mesh)
      (reordered-vertex-data mesh (mapcar #'%decode-sf3-vertex-attribute attributes))
      :vertex-attributes attributes
      :material textures)
     target :if-exists if-exists)))

(defmethod save-model ((model model) target (type (eql :sf3)) &rest args &key &allow-other-keys)
  (loop for name being the hash-keys of (meshes model) using (hash-value mesh)
        for path = (merge-pathnames (make-pathname :name (if (consp name)
                                                             (format NIL "~a-~a" (car name) (cdr name))
                                                             (princ-to-string name)))
                                    target)
        do (apply #'save-model mesh path type args)))
