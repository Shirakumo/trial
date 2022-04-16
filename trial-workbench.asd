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
  :depends-on (:trial-glfw
               :trial-jpeg
               :trial-png))
