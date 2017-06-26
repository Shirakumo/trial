#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)

(asdf:defsystem trial-physics
  :version "1.0.0"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Implementations for physics simulation to be used with Trial Game Engine."
  :homepage "https://github.com/Shirakumo/trial"
  :components ((:file "package")
               (:file "verlet" :depends-on ("package")))
  :depends-on (:trial))
