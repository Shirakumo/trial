(asdf:defsystem trial-selftest
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname #+linux "trial-linux.run"
  #+darwin "trial-macos.o"
  #+win32 "trial-windows"
  #+(and bsd (not darwin)) "trial-bsd.run"
  #-(or linux bsd win32) "trial"
  :entry-point "org.shirakumo.fraf.trial.selftest:run"
  :components ((:file "selftest"))
  :depends-on (#-nx :trial-glfw
               #+nx :trial-nxgl
               :trial-harmony
               :trial-png
               :usocket
               :dns-client)
  :perform (asdf:test-op (op c) (uiop:symbol-call :org.shirakumo.fraf.trial.selftest :run)))
