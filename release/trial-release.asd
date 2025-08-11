(asdf:defsystem trial-release
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Release management for Trial games."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :components ((:file "package")
               (:file "toolkit")
               (:file "build")
               (:file "bundle")
               (:file "upload")
               (:file "release"))
  :depends-on (:alexandria
               :cl-ppcre
               :pathname-utils
               :filesystem-utils
               :zippy
               :deploy
               :cl-ftp
               :dexador
               :north-drakma
               :trivial-ssh))
