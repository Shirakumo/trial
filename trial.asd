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
  :license "zlib"
  :description "A flexible and extensible video game engine."
  :homepage "https://Shirakumo.github.io/trial/"
  :bug-tracker "https://github.com/Shirakumo/trial/issues"
  :source-control (:git "https://github.com/Shirakumo/trial.git")
  :components ((:file "package")
               (:file "achievements" :depends-on ("package" "event-loop" "main"))
               (:file "actions" :depends-on ("package" "toolkit"))
               (:file "array-container" :depends-on ("package" "container"))
               (:file "asset" :depends-on ("package" "toolkit" "resource" "resource-generator"))
               (:file "asset-pool" :depends-on ("package" "asset"))
               (:file "async" :depends-on ("package" "main"))
               (:file "bag" :depends-on ("package" "container"))
               (:file "camera" :depends-on ("package" "container" "input" "transforms"))
               (:file "capture" :depends-on ("package" "toolkit" "serialize" "event-loop" "input"))
               (:file "conditions" :depends-on ("package"))
               (:file "container" :depends-on ("package" "toolkit" "transforms"))
               (:file "context" :depends-on ("package"))
               (:file "controller" :depends-on ("package" "mapping" "input" "container" "asset" "text"))
               (:file "data-pointer" :depends-on ("package" "static-vector"))
               (:file "debug" :depends-on ("package" "shader-entity" "text" ("physics" "primitives")))
               (:file "deploy" :depends-on ("package" "gamepad"))
               (:file "display" :depends-on ("package" "context" "render-loop" "power" "transforms" "gl-features"))
               (:file "documentation" :depends-on ("package"))
               (:file "effects" :depends-on ("package" "shader-pass"))
               (:file "error-handling" :depends-on ("package" "toolkit"))
               (:file "event-loop" :depends-on ("package" "container" "queue" "toolkit"))
               (:file "features" :depends-on ("package"))
               (:file "fps" :depends-on ("package" ("assets" "image") ("assets" "mesh") "loader"))
               (:file "gamepad" :depends-on ("package" "event-loop" "toolkit"))
               (:file "geometry" :depends-on ("package" "toolkit" "type-info" "static-vector" ("resources" "vertex-array") ("resources" "vertex-buffer")))
               (:file "geometry-clipmap" :depends-on ("package" "geometry-shapes" "shader-entity"))
               (:file "geometry-shapes" :depends-on ("package" "geometry" "asset-pool" ("assets" "mesh")))
               (:file "gl-features" :depends-on ("package"))
               (:file "gl-struct" :depends-on ("package" "type-info"))
               (:file "hash-table-container" :depends-on ("package" "container"))
               (:file "helpers" :depends-on ("package" "container" "transforms" "shader-entity" "shader-pass" "asset" "resources" "loader"))
               (:file "hdr" :depends-on ("package" "shader-pass"))
               (:file "input" :depends-on ("package" "event-loop"))
               (:file "interpolation" :depends-on ("package"))
               (:file "language" :depends-on ("package" "toolkit" "settings"))
               (:file "lines" :depends-on ("package" "helpers" "shader-entity" "geometry"))
               (:file "list-container" :depends-on ("package" "container"))
               (:file "layered-container" :depends-on ("package" "container"))
               (:file "loader" :depends-on ("package" "resource" "asset"))
               (:file "main" :depends-on ("package" "display" "toolkit" "scene" "pipeline"))
               (:file "mapping" :depends-on ("package" "event-loop" "actions" "toolkit" "input"))
               (:file "os-resources" :depends-on ("package"))
               (:file "parallax" :depends-on ("package" "shader-entity" "assets"))
               (:file "particle" :depends-on ("package" "shader-entity" "resources"))
               (:file "pipeline" :depends-on ("package" "event-loop" "toolkit" "shader-pass"))
               (:file "pipelined-scene" :depends-on ("package" "pipeline" "scene" "loader"))
               (:file "pixel-pipeline" :depends-on ("package" "pipeline"))
               (:file "power" :depends-on ("package"))
               (:file "prompt" :depends-on ("package"))
               (:file "queue" :depends-on ("package"))
               (:file "rails" :depends-on ("package" "container" "helpers"))
               (:file "render-loop" :depends-on ("package" "toolkit"))
               (:file "render-texture" :depends-on ("package" "pipeline" "container"))
               (:file "resource" :depends-on ("package" "context"))
               (:file "resource-generator" :depends-on ("package"))
               (:file "scene-buffer" :depends-on ("package" "scene" "render-texture"))
               (:file "scene" :depends-on ("package" "event-loop" "container" "camera"))
               (:file "selection-buffer" :depends-on ("package" "render-texture" "scene" "effects" "loader" "gl-features"))
               (:file "serialize" :depends-on ("package"))
               (:file "settings" :depends-on ("package" "toolkit"))
               (:file "shader-entity" :depends-on ("package" "container" "event-loop" "loader"))
               (:file "shader-pass" :depends-on ("package" "shader-entity" "resource" ("resources" "framebuffer") ("resources" "shader-program") "scene" "loader" "context" "geometry-shapes" "camera"))
               (:file "skybox" :depends-on ("package" "shader-entity" "transforms"))
               (:file "sprite" :depends-on ("package" "shader-entity" "helpers" ("assets" "sprite-data")))
               (:file "ssao" :depends-on ("package" "shader-pass" "transforms"))
               (:file "static-vector" :depends-on ("package"))
               (:file "text" :depends-on ("package" "helpers" ("assets" "image")))
               (:file "tile-layer" :depends-on ("package" "helpers" ("assets" "tile-data")))
               (:file "toolkit" :depends-on ("package" "conditions"))
               (:file "transforms" :depends-on ("package"))
               (:file "type-info" :depends-on ("package" "toolkit" "data-pointer"))
               (:module "animation"
                :depends-on ("package" "shader-entity" "helpers")
                :components ((:file "asset" :depends-on ("clip"))
                             (:file "clip" :depends-on ("track"))
                             (:file "entity" :depends-on ("mesh" "skeleton" "clip" "ik"))
                             (:file "ik" :depends-on ("skeleton"))
                             (:file "mesh" :depends-on ("pose"))
                             (:file "pose")
                             (:file "skeleton" :depends-on ("pose"))
                             (:file "track")))
               (:module "physics"
                :depends-on ("package" "conditions")
                :components ((:file "toolkit")
                             (:file "constants" :depends-on ("toolkit"))
                             (:file "core" :depends-on ("toolkit"))
                             (:file "hit-detection" :depends-on ("primitives"))
                             (:file "inertia-tensors" :depends-on ("core"))
                             (:file "particle" :depends-on ("core"))
                             (:file "rigidbody" :depends-on ("primitives"))
                             (:file "primitives" :depends-on ("core"))
                             (:file "ray" :depends-on ("primitives"))
                             (:file "resolution" :depends-on ("rigidbody"))))
               (:module "renderer"
                :depends-on ("package" "shader-pass" "helpers" "gl-struct" "transforms")
                :components ((:file "lights")
                             (:file "materials")
                             (:file "pbr" :depends-on ("standard-renderer" "shadow-map"))
                             (:file "phong" :depends-on ("standard-renderer" "shadow-map"))
                             (:file "shadow-map" :depends-on ("standard-renderer"))
                             (:file "standard-renderer" :depends-on ("lights" "materials"))))
               (:module "resources"
                :depends-on ("package" "resource" "toolkit" "data-pointer")
                :components ((:file "buffer-object")
                             (:file "compute-shader" :depends-on ("shader-program"))
                             (:file "framebuffer")
                             (:file "shader-program")
                             (:file "shader")
                             (:file "shader-storage-buffer" :depends-on ("uniform-buffer"))
                             (:file "struct-buffer" :depends-on ("buffer-object" (:.. "gl-struct")))
                             (:file "texture")
                             (:file "uniform-buffer" :depends-on ("struct-buffer"))
                             (:file "vertex-array")
                             (:file "vertex-buffer" :depends-on ("buffer-object"))
                             (:file "vertex-struct-buffer" :depends-on ("struct-buffer"))))
               (:module "assets"
                :depends-on ("package" "asset" "resources" "data-pointer")
                :components ((:file "image")
                             (:file "mesh" :depends-on ((:.. ("physics" "primitives"))))
                             (:file "sprite-data" :depends-on ("image"))
                             (:file "static")
                             (:file "tile-data" :depends-on ("image"))
                             (:file "uniform-block"))))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:alexandria
               :atomics
               :3d-vectors
               :3d-matrices
               :3d-quaternions
               :3d-transforms
               :3d-spaces
               :verbose
               :deploy
               :closer-mop
               :trivial-garbage
               :trivial-indent
               :trivial-extensible-sequences
               :bordeaux-threads
               :cl-opengl
               :cl-gamepad
               :cl-ppcre
               :pathname-utils
               :documentation-utils
               :for
               :flow
               :glsl-toolkit
               :fast-io
               :ieee-floats
               :float-features
               :lquery
               :static-vectors
               :mmap
               :messagebox
               :nibbles
               :form-fiddle
               :lambda-fiddle
               :com.inuoe.jzon
               :zpng
               :system-locale
               :language-codes
               :promise
               :simple-tasks
               :lru-cache
               (:feature :windows :com-on)))
