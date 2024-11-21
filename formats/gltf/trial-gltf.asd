(asdf:defsystem trial-gltf
  :serial T
  :components ((:file "package")
               (:file "reader")
               (:file "animation")
               (:file "material")
               (:file "mesh")
               (:file "physics")
               (:file "scene")
               (:file "optimize"))
  :depends-on (:trial :cl-gltf))
