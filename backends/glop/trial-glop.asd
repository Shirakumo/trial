(asdf:defsystem trial-glop
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "GLOP backend for Trial."
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :serial T
  :components ((:file "package")
               (:file "context"))
  :depends-on (:glop
               :trial
               :trivial-main-thread))
