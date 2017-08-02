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
               (:file "subject-chooser")
               (:file "subject-class-chooser"))
  :depends-on (:trial
               :qtools
               :qtcore
               :qtgui
               :qtools-ui-listing))
