(asdf:defsystem trial-examples
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname #+linux "trial-examples.run"
  #+darwin "trial-examples.o"
  #+win32 "trial-examples"
  #+(and bsd (not darwin)) "trial-examples-bsd.run"
  #-(or linux bsd win32) "trial-examples"
  :entry-point "trial-examples::launch"
  :components ((:file "trial-examples")
               (:file "triangle")
               (:file "vclip"))
  :depends-on (:trial-glfw
               :trial-alloy
               :trial-jpeg-turbo
               :trial-png
               :trial-gltf
               :trial-hdr
               :trial-assets-gltf))
