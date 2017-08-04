#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trial-editor
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A flexible and extensible video game engine."
  :homepage "https://github.com/Shirakumo/trial"
  :components ((:file "package")
               (:file "inspector")
               (:file "object-inspector")
               (:file "hash-table-inspector")
               (:file "array-inspector")
               (:file "cons-inspector")
               (:file "list-inspector")
               (:file "symbol-inspector")
               (:file "package-inspector")
               (:file "pathname-inspector")
               (:file "subject-chooser")
               (:file "subject-class-chooser")
               (:file "scene-graph"))
  :depends-on (:trial
               :qtools
               :qtcore
               :qtgui
               :qtools-ui-listing
               :array-utils
               :cl-ppcre))
