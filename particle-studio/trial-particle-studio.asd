(asdf:defsystem trial-particle-studio
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname #+linux "trial-particle-studio.run"
  #+darwin "trial-particle-studio.o"
  #+win32 "trial-particle-studio"
  #+(and bsd (not darwin)) "trial-particle-studio-bsd.run"
  #-(or linux bsd win32) "trial-particle-studio"
  :entry-point "trial-particle-studio::main"
  :serial T
  :components ((:file "package")
               (:file "ui")
               (:file "main"))
  :depends-on (:trial-glfw
               :alloy-constraint
               :trial-alloy
               :trial-jpeg-turbo
               :trial-png
               :trial-gltf
               :trial-assets
               :file-select))
