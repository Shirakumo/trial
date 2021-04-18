#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(asdf:defsystem trial-physics
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :license "zlib"
  :description "Verlet integrated physics"
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :components ((:file "package")
               (:file "physics")
               (:file "verlet")
               (:file "edge")
               (:file "entity")
               (:file "shape"))
  :depends-on (:trial))
