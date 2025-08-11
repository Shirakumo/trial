(asdf:defsystem trial-gog
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A flexible and extensible video game engine, GOG Galaxy integration."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :components ((:file "gog")
               (:file "achievements"))
  :depends-on (:trial
               :cl-gog-galaxy))
