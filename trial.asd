#|
 This file is a part of trial
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
               (:file "toolkit")
               (:file "framebuffer")
               (:file "context")
               (:file "storage")
               (:file "assets")
               (:file "entity")
               (:file "event-loop")
               (:file "scene")
               (:file "windowing")
               (:file "flare")
               (:file "subjects")
               (:file "sprite")
               (:file "octree")
               (:file "camera")
               (:file "selectable")
               (:file "input-tables")
               (:file "input")
               (:file "mapping")
               (:file "controller")
               (:file "player"))
  :depends-on (:alexandria
               :3d-vectors
               :verbose
               :qtools
               :qtcore
               :qtgui
               :qtopengl
               :cl-opengl
               :closer-mop
               :trivial-garbage
               :bordeaux-threads
               :wavefront-loader
               :cl-gamepad
               :pathname-utils
               :flare))
