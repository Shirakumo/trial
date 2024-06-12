(asdf:defsystem trial-workbench
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname #+linux "workbench-linux.run"
  #+darwin "workbench-macos.o"
  #+win32 "workbench-windows"
  #+(and bsd (not darwin)) "workbench-bsd.run"
  #-(or linux bsd win32) "workbench"
  :entry-point "workbench::launch"
  :components ((:file "workbench"))
  :depends-on (#-nx :trial-glfw
               #+nx :trial-nxgl
               :trial-jpeg-turbo
               :trial-assets
               :trial-alloy
               :trial-png
               :trial-gltf
               :trial-hdr))
