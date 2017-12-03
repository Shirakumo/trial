#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#



(defmethod asdf/find-component:resolve-dependency-combination (component (combinator (eql :..)) args)
  (asdf/find-component:resolve-dependency-spec
   (asdf:component-parent component) (first args)))

(defmethod asdf/find-component:resolve-dependency-combination (component (combinator string) args)
  (asdf:find-component
   (asdf:find-component (asdf:component-parent component) combinator)
   (first args)))

(asdf:defsystem trial
  :version "1.2.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A flexible and extensible video game engine."
  :homepage "https://github.com/Shirakumo/trial"
  :components ((:file "package")
               (:file "array-container" :depends-on ("package"))
               (:file "asset" :depends-on ("package" "toolkit" "context"))
               (:file "asset-pool" :depends-on ("package" "asset" "window"))
               (:file "attributes" :depends-on ("package"))
               (:file "camera" :depends-on ("package" "subject" "helpers"))
               (:file "context" :depends-on ("package"))
               (:file "controller" :depends-on ("package" "mapping" "input" "subject" ("assets" "font")))
               (:file "deploy" :depends-on ("package" "gamepad"))
               (:file "display" :depends-on ("package" "context" "renderable"))
               (:file "effects" :depends-on ("package" "shader-pass"))
               (:file "entity" :depends-on ("package"))
               (:file "event-loop" :depends-on ("package" "entity"))
               (:file "features" :depends-on ("package"))
               (:file "flare" :depends-on ("package" "transforms"))
               (:file "fullscreenable" :depends-on ("package" "display"))
               (:file "gamepad" :depends-on ("package" "event-loop" "toolkit"))
               (:file "geometry" :depends-on ("package" "toolkit" "static-vector" ("assets" "vertex-array")))
               (:file "geometry-clipmap" :depends-on ("package" "geometry-shapes" "shader-subject"))
               (:file "geometry-shapes" :depends-on ("package" "geometry" "asset-pool"))
               (:file "helpers" :depends-on ("package" "entity" "transforms" "shader-subject" "shader-pass" "asset"))
               (:file "input" :depends-on ("package" "event-loop" "retention"))
               (:file "layer-set" :depends-on ("package"))
               (:file "main" :depends-on ("package" "display" "window" "toolkit" "scene" "pipeline"))
               (:file "mapping" :depends-on ("package" "event-loop" "toolkit"))
               (:file "pipeline" :depends-on ("package" "event-loop" "toolkit"))
               (:file "rails" :depends-on ("package" "subject" "helpers"))
               (:file "render-texture" :depends-on ("package" "pipeline" "entity"))
               (:file "renderable" :depends-on ("package" "toolkit"))
               (:file "retention" :depends-on ("package" "event-loop"))
               (:file "scene-buffer" :depends-on ("package" "scene" "render-texture"))
               (:file "scene" :depends-on ("package" "event-loop" "entity"))
               (:file "selection-buffer" :depends-on ("package" "render-texture" "scene" "effects"))
               (:file "shader-entity" :depends-on ("package" "entity"))
               (:file "shader-pass" :depends-on ("package" "shader-subject" "asset" "scene"))
               (:file "shader-subject" :depends-on ("package" "shader-entity" "subject"))
               (:file "skybox" :depends-on ("package" "shader-subject" "transforms"))
               (:file "sprite" :depends-on ("package" "shader-subject" "helpers"))
               (:file "static-vector" :depends-on ("package"))
               (:file "subject" :depends-on ("package" "event-loop"))
               (:file "toolkit" :depends-on ("package"))
               (:file "transforms" :depends-on ("package"))
               (:file "window" :depends-on ("package"))
               ;; Testing, remove for production.
               (:file "workbench" :depends-on ("assets" "asset-pool" "formats" "main" "helpers" "geometry-clipmap" "ui"))
               (:module "ui"
                :depends-on ("package" "shader-entity" "helpers" "input")
                :components ((:file "package")
                             (:file "widget" :depends-on ("package"))
                             (:file "pane" :depends-on ("package" "widget"))
                             (:file "layout" :depends-on ("package" "widget"))
                             (:file "input" :depends-on ("package" "widget"))
                             (:file "elements" :depends-on ("package" "widget" "input"))
                             (:file "text-field" :depends-on ("package" "elements"))
                             (:file "ui-window" :depends-on ("package" "pane" "elements"))))
               (:module "assets"
                :depends-on ("package" "asset" "toolkit")
                :components ((:file "texture")
                             (:file "shader")
                             (:file "shader-program" :depends-on ("shader"))
                             (:file "vertex-buffer" :depends-on ((:.. "static-vector")))
                             (:file "vertex-array" :depends-on ("vertex-buffer"))
                             (:file "framebuffer" :depends-on ("vertex-array" "texture"))
                             (:file "mesh" :depends-on ((:.. "geometry") (:.. "static-vector")))
                             (:file "font" :depends-on ((:.. "shader-pass") (:.. "helpers")))))
               (:module "formats"
                :depends-on ("package" "geometry" "static-vector")
                :components ((:file "vertex-format")
                             (:file "collada"))))
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
               :lquery
               :static-vectors))
