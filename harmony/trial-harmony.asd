(asdf:defsystem trial-harmony
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Integration with the Harmony sound engine"
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :components ((:file "package")
               (:file "asset")
               (:file "main")
               (:file "deploy"))
  :depends-on (:trial
               :harmony
               :uiop
               :cl-ppcre))
