(asdf:defsystem :trial-animation
  :depends-on (:trial-glfw
               :cl-gltf)
  :components ((:file "package")
               (:file "interpolation")
               (:file "track")
               (:file "clip")
               (:file "pose")
               (:file "mesh")
               (:file "gltf")
               (:file "asset")
               (:file "entity")
               (:file "workbench")))
