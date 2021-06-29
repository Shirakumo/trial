#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem trial-harmony
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Integration with the Harmony sound engine"
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :components ((:file "package")
               (:file "asset")
               (:file "deploy"))
  :depends-on (:trial
               :harmony
               :uiop))
