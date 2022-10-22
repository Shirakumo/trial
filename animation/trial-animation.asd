(asdf:defsystem :trial-animation
  :depends-on (:trial-glfw
               :trial-png
               :cl-gltf)
  :components ((:file "package")
               (:file "track")
               (:file "clip")
               (:file "pose")
               (:file "skeleton")
               (:file "mesh")
               (:file "gltf")
               (:file "asset")
               (:file "entity")
               (:file "workbench")))
