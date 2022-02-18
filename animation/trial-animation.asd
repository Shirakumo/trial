(asdf:defsystem :trial-animation
  :depends-on (:trial
               :cl-gltf)
  :components ((:file "package")
               (:file "interpolation")
               (:file "track")
               (:file "clip")
               (:file "pose")
               (:file "gltf")))
