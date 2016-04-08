#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trial-editor
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Editor component for the Trial engine"
  :homepage "https://github.com/Shirakumo/trial"
  :serial T
  :components ((:file "editor")
               (:file "console")
               (:file "inspector"))
  :depends-on (:trial
               :verbose
               :qtools
               :qtcore
               :qtgui
               :closer-mop
               :pathname-utils
               :cl-ppcre
               :trivial-gray-streams))
