(asdf:defsystem trial-glfw
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "GLFW3 backend for Trial."
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :serial T
  :components ((:file "package")
               (:file "queue")
               (:file "context"))
  :depends-on (:cl-glfw3
               :atomics
               :trial
               :pathname-utils
               :trivial-main-thread
               :trivial-garbage))
