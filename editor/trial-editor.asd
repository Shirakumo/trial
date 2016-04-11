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
  :components ((:file "console")
               (:file "inspector")
               (:file "object-tree")
               (:file "asset-browser")
               (:file "editor"))
  :depends-on (:trial
               :verbose
               :qtools
               :qtcore
               :qtgui
               :closer-mop
               :pathname-utils
               :qtools-ui-options
               :qtools-ui-repl
               :qtools-ui-debugger))
