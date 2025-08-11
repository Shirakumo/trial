(asdf:defsystem trial-sdl2
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "SDL2 backend for Trial."
  :homepage "https://shirakumo.org/docs/trial/"
  :bug-tracker "https://shirakumo.org/project/trial/issues"
  :source-control (:git "https://shirakumo.org/project/trial.git")
  :serial T
  :components ((:file "package")
               (:file "keycodes")
               (:file "context"))
  :depends-on (:sdl2
               :trial
               :trivial-main-thread
               :trivial-garbage))
