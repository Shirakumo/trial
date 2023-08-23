(asdf:defsystem trial-harmony
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Integration with the Harmony sound engine"
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :components ((:file "package")
               (:file "asset")
               (:file "main")
               (:file "deploy"))
  :depends-on (:trial
               :harmony
               :uiop
               :cl-ppcre))
