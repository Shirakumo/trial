#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem trial-release
  :version "1.2.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Release management for Trial games."
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :components ((:file "package")
               (:file "toolkit")
               (:file "build")
               (:file "release"))
  :depends-on (:alexandria
               :cl-ppcre
               :pathname-utils
               :zippy
               :deploy
               :cl-ftp
               :dexador
               :north
               :trivial-ssh))
