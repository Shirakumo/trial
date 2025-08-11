(asdf:defsystem trial-nxgl
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "NXGL backend for Trial."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :serial T
  :components ((:file "nxgl")
               (:file "package")
               (:file "input-tables")
               (:file "context"))
  :depends-on (:cffi
               :trial))
