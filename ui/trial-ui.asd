#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trial-ui
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "In-game UI elements for the Trial engine."
  :homepage "https://github.com/Shirakumo/trial"
  :components ((:file "package")
               (:file "base")
               (:file "layout")
               (:file "elements"))
  :depends-on (:trial))
