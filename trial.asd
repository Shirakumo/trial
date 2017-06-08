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
  :description "A flexible and extensible video game engine."
  :homepage "https://github.com/Shirakumo/trial"
  :serial T
  :components ((:file "package")
               (:file "features")
               (:file "toolkit")
               (:file "transforms")
               (:file "attributes")
               (:file "entity")
               (:file "savestate")
               (:file "layer-set")
               (:file "window")
               (:file "fullscreenable")
               (:file "renderable")
               (:file "context")
               (:file "display")
               (:file "dialog-system")
               (:file "asset")
               (:file "asset-pool")
               (:file "vertex-format")
               (:file "collada")
               (:file "geometry")
               (:file "helpers")
               (:file "event-loop")
               (:file "subject")
               (:file "shader-subject")
               (:file "scene")
               (:file "shader-pass")
               (:file "effects")
               (:file "pipeline")
               (:file "subjects")
               (:file "camera")
               (:file "flare")
               (:file "mapping")
               (:file "retention")
               (:file "physics")
               (:file "input")
               (:file "gamepad")
               (:file "controller")
               (:file "main")
               (:file "deploy")
               ;; Testing, remove for production.
               (:file "workbench"))
  :depends-on (:alexandria
               :3d-vectors
               :3d-matrices
               :verbose
               :deploy
               :closer-mop
               :trivial-garbage
               :trivial-indent
               :bordeaux-threads
               :cl-opengl
               :cl-gamepad
               :cl-monitors
               :cl-soil
               :cl-fond
               :pathname-utils
               :flare
               :for
               :flow
               :glsl-toolkit
               :fast-io
               :ieee-floats
               :lquery))
