(asdf:defsystem trial-nxgl
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "NXGL backend for Trial."
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :serial T
  :components ((:file "package")
               (:file "input-tables")
               (:file "context"))
  :depends-on (:nxgl
               :trial))
