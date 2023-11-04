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
               (:file "collision")
               (:file "raycast")
               (:file "cpu-particle")
               (:file "gpu-particle")
               (:file "spatial-query")
               (:file "rigging")
               (:file "video")
               (:file "pbr")
               (:file "selection")
               (:file "physics")
               (:file "convex-physics"))
  :depends-on (#-nx :trial-glfw
               #+nx :trial-nxgl
               :trial-theora
               :trial-alloy
               :trial-jpeg-turbo
               :trial-png
               :trial-gltf
               :trial-obj
               :trial-hdr
               :trial-assets
               :file-select))
