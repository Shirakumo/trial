#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trial-glop
  :version "1.2.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "GLOP backend for Trial."
  :homepage "https://github.com/Shirakumo/trial"
  :serial T
  :components ((:file "package")
               (:file "context"))
  :depends-on (:glop
               :trial
               :trivial-main-thread))
