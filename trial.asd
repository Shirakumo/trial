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
               (:file "asset" :depends-on ("package" "toolkit" "resource" "context"))
               (:file "asset-pool" :depends-on ("package" "asset"))
               (:file "attributes" :depends-on ("package"))
               (:file "camera" :depends-on ("package" "subject" "helpers"))
               (:file "context" :depends-on ("package"))
               (:file "controller" :depends-on ("package" "mapping" "input" "subject" "asset" "text"))
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
               (:file "geometry-shapes" :depends-on ("package" "geometry" "asset-pool" ("assets" "mesh")))
               (:file "helpers" :depends-on ("package" "entity" "transforms" "shader-subject" "shader-pass" "asset" "resources"))
               (:file "input" :depends-on ("package" "event-loop" "retention"))
               (:file "layer-set" :depends-on ("package"))
               (:file "loader" :depends-on ("package" "scene" "resource"))
               (:file "main" :depends-on ("package" "display" "toolkit" "scene" "pipeline" "window"))
               (:file "mapping" :depends-on ("package" "event-loop" "toolkit"))
               (:file "pipeline" :depends-on ("package" "event-loop" "toolkit"))
               (:file "pipelined-scene" :depends-on ("package" "pipeline" "scene" "loader"))
               (:file "rails" :depends-on ("package" "subject" "helpers"))
               (:file "redefinition-notifying-class" :depends-on ("package"))
               (:file "render-texture" :depends-on ("package" "pipeline" "entity"))
               (:file "renderable" :depends-on ("package" "toolkit"))
               (:file "resource" :depends-on ("package" "context"))
               (:file "retention" :depends-on ("package" "event-loop"))
               (:file "scene-buffer" :depends-on ("package" "scene" "render-texture"))
               (:file "scene" :depends-on ("package" "event-loop" "entity"))
               (:file "selection-buffer" :depends-on ("package" "render-texture" "scene" "effects" "loader"))
               (:file "shader-entity" :depends-on ("package" "entity" "redefinition-notifying-class"))
               (:file "shader-pass" :depends-on ("package" "shader-subject" "resource" "scene" "loader" "context"))
               (:file "shader-subject" :depends-on ("package" "shader-entity" "subject"))
               (:file "skybox" :depends-on ("package" "shader-subject" "transforms"))
               (:file "sprite" :depends-on ("package" "shader-subject" "helpers"))
               (:file "static-vector" :depends-on ("package"))
               (:file "subject" :depends-on ("package" "event-loop"))
               (:file "text" :depends-on ("package" "shader-entity" "helpers" ("assets" "font")))
               (:file "toolkit" :depends-on ("package"))
               (:file "transforms" :depends-on ("package"))
               (:file "window" :depends-on ("package"))
               (:file "physics/package" :depends-on ("package"))
               (:file "physics/math" :depends-on ("physics/package"))
               (:file "physics/physics" :depends-on ("physics/math"))
               (:file "physics/constraint" :depends-on ("physics/math"))
               (:file "physics/verlet" :depends-on ("physics/physics" "physics/constraint"))
               (:file "physics/collision" :depends-on ("physics/verlet"))
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
               (:module "resources"
                :depends-on ("package" "resource" "toolkit")
                :components ((:file "framebuffer")
                             (:file "shader-program")
                             (:file "shader")
                             (:file "texture")
                             (:file "vertex-array")
                             (:file "vertex-buffer")))
               (:module "assets"
                :depends-on ("package" "asset" "resources")
                :components ((:file "font")
                             (:file "image")
                             (:file "mesh")))
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
               :cl-ppcre
               :pathname-utils
               :flare
               :for
               :flow
               :glsl-toolkit
               :fast-io
               :ieee-floats
               :lquery
               :static-vectors))
