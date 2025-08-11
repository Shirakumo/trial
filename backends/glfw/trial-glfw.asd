(asdf:defsystem trial-glfw
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "GLFW3 backend for Trial."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :serial T
  :components ((:file "package")
               (:file "queue")
               (:file "context"))
  :depends-on (:glfw
               :atomics
               :trial
               :trivial-main-thread))
