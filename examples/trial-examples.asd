(asdf:defsystem trial-examples
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname #+linux "trial-examples.run"
  #+darwin "trial-examples.o"
  #+win32 "trial-examples"
  #+(and bsd (not darwin)) "trial-examples-bsd.run"
  #-(or linux bsd win32) "trial-examples"
  :entry-point "trial-examples::main"
  :components ((:file "trial-examples")
               (:file "triangle")
               (:file "collision")
               (:file "raycast")
               (:file "particle-common")
               (:file "cpu-particle")
               (:file "gpu-particle")
               (:file "spatial-query")
               (:file "rigging")
               (:file "video")
               (:file "pbr")
               (:file "selection")
               (:file "physics")
               (:file "convex-physics")
               (:file "scene-loader")
               (:file "decomposition")
               (:file "sprite")
               (:file "system-info")
               (:file "save-data")
               (:file "audio")
               (:file "tilemap")
               (:file "shader")
               (:file "post-processing")
               (:file "culling")
               (:file "letterbox")
               (:file "repl"))
  :depends-on (#-nx :trial-glfw
               #+nx :trial-nxgl
               :alloy-constraint
               :trial-theora
               :trial-alloy
               :trial-jpeg-turbo
               :trial-png
               :trial-gltf
               :trial-obj
               :trial-hdr
               :trial-assets
               :trial-harmony
               :cl-mixed-wav
               :cl-mixed-vorbis
               :file-select))

(asdf:defsystem trial-examples/release
  :components ((:file "release"))
  :depends-on (:trial-release)
  :perform (asdf:build-op (op c) (uiop:symbol-call :org.shirakumo.fraf.trial.release :make)))
