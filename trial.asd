#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trial
  :version "1.2.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Trial run for the upcoming Ludum Dare 35"
  :homepage "https://github.com/Shirakumo/trial"
  :serial T
  :components ((:file "package")
               (:file "event-loop")
               (:file "windowing")
               (:file "game-object"))
  :depends-on (:qtools
               :qtcore
               :qtgui
               :qtopengl
               :deeds
               :cl-opengl
               :verbose))
