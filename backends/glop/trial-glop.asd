(asdf:defsystem trial-glop
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "GLOP backend for Trial."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :serial T
  :components ((:file "package")
               (:file "context"))
  :depends-on (:glop
               :trial
               :trivial-main-thread))
