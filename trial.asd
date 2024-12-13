(defmethod asdf/find-component:resolve-dependency-combination (component (combinator (eql :..)) args)
  (asdf/find-component:resolve-dependency-spec
   (asdf:component-parent component) (first args)))

(defmethod asdf/find-component:resolve-dependency-combination (component (combinator string) args)
  (asdf:find-component
   (asdf:find-component (asdf:component-parent component) combinator)
   (first args)))

(asdf:defsystem trial
  :version "1.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               (:file "async" :depends-on ("package"))
               (:file "bag" :depends-on ("package" "container"))
               (:file "bounds" :depends-on ("package" "container"))
               (:file "camera" :depends-on ("package" "container" "input" "transforms" "helpers" "bounds" ("physics" "rigidbody")))
               (:file "capture" :depends-on ("package" "toolkit" "serialize" "event-loop" "input"))
               (:file "command-line" :depends-on ("package" "toolkit"))
               (:file "conditions" :depends-on ("package"))
               (:file "container" :depends-on ("package" "toolkit" "transforms"))
               (:file "context" :depends-on ("package" "event-loop"))
               (:file "controller" :depends-on ("package" "mapping" "input" "container" "asset" "text"))
               (:file "debug" :depends-on ("package" "shader-entity" "text" ("physics" "primitives")))
               (:file "deploy" :depends-on ("package" "gamepad" "os-resources"))
               (:file "display" :depends-on ("package" "context" "render-loop" "power" "transforms" "gl-features"))
               (:file "documentation" :depends-on ("package" "physics" "animation" "geometry-shapes" "renderer"))
               (:file "effects" :depends-on ("package" "shader-pass"))
               (:file "error-handling" :depends-on ("package" "toolkit"))
               (:file "event-loop" :depends-on ("package" "queue" "toolkit"))
               (:file "features" :depends-on ("package"))
               (:file "flicker" :depends-on ("package"))
               (:file "fps" :depends-on ("package" ("assets" "mesh") "text" "loader" "texture-source" ("formats" "raw")))
               (:file "gamepad" :depends-on ("package" "event-loop" "toolkit"))
               (:file "geometry" :depends-on ("package" "toolkit" "type-info" ("resources" "vertex-array") ("resources" "vertex-buffer")))
               (:file "geometry-shapes" :depends-on ("package" "geometry" "asset-pool" ("assets" "mesh")))
               (:file "gl-features" :depends-on ("package"))
               (:file "gl-struct" :depends-on ("package" "type-info"))
               (:file "hash-table-container" :depends-on ("package" "container"))
               (:file "helpers" :depends-on ("package" "container" "transforms" "shader-entity" "asset" "asset-pool" "resources" "loader" "geometry" "geometry-shapes" "loader"))
               (:file "input" :depends-on ("package" "event-loop"))
               (:file "interpolation" :depends-on ("package"))
               (:file "language" :depends-on ("package" "toolkit" "settings"))
               (:file "lines" :depends-on ("package" "helpers" "shader-entity" "geometry"))
               (:file "list-container" :depends-on ("package" "container"))
               (:file "layered-container" :depends-on ("package" "container"))
               (:file "loader" :depends-on ("package" "resource" "asset" "async"))
               (:file "main" :depends-on ("package" "display" "toolkit" "scene" "pipeline" "mapping"))
               (:file "mapping" :depends-on ("package" "event-loop" "actions" "toolkit" "input"))
               (:file "model" :depends-on ("package"))
               (:file "os-resources" :depends-on ("package"))
               (:file "parallax" :depends-on ("package" "shader-entity" "assets"))
               (:file "pipeline" :depends-on ("package" "event-loop" "toolkit" "shader-pass" "settings"))
               (:file "pipelined-scene" :depends-on ("package" "pipeline" "scene" "loader"))
               (:file "pixel-pipeline" :depends-on ("package" "pipeline"))
               (:file "power" :depends-on ("package"))
               (:file "prefab" :depends-on ("package" "model"))
               (:file "prompt" :depends-on ("package"))
               (:file "queue" :depends-on ("package"))
               (:file "random" :depends-on ("package"))
               (:file "render-loop" :depends-on ("package" "toolkit"))
               (:file "resource" :depends-on ("package" "context"))
               (:file "resource-generator" :depends-on ("package"))
               (:file "save-data" :depends-on ("package" "resources"))
               (:file "scene" :depends-on ("package" "event-loop" "container" "camera"))
               (:file "selection" :depends-on ("package" "shader-pass"))
               (:file "serialize" :depends-on ("package"))
               (:file "settings" :depends-on ("package" "toolkit" "error-handling"))
               (:file "shader-entity" :depends-on ("package" "container" "event-loop" "loader" "geometry"))
               (:file "shader-pass" :depends-on ("package" "shader-entity" "helpers" "resource" ("resources" "framebuffer") ("resources" "shader-program") "scene" "loader" "context" "geometry-shapes" "camera"))
               (:file "skybox" :depends-on ("package" "shader-entity" "transforms"))
               (:file "sprite" :depends-on ("package" "shader-entity" "helpers" ("assets" "sprite-data")))
               (:file "text" :depends-on ("package" "helpers" ("assets" "image")))
               (:file "texture-source" :depends-on ("package" "toolkit"))
               (:file "tile-layer" :depends-on ("package" "helpers" ("assets" "tile-data")))
               (:file "toolkit" :depends-on ("package" "conditions"))
               (:file "transforms" :depends-on ("package" "context"))
               (:file "type-info" :depends-on ("package" "toolkit"))
               (:file "video" :depends-on ("package" "helpers"))
               (:module "animation"
                :depends-on ("package" "shader-entity" "helpers" "model")
                :components ((:file "clip" :depends-on ("track"))
                             (:file "controller" :depends-on ("mesh" "skeleton" "clip" "ik"))
                             (:file "entity" :depends-on ("controller"))
                             (:file "ik" :depends-on ("skeleton"))
                             (:file "mesh" :depends-on ("pose"))
                             (:file "pose" :depends-on ("clip" (:.. "lines")))
                             (:file "skeleton" :depends-on ("pose"))
                             (:file "track")))
               (:module "formats"
                :depends-on ("package")
                :components ((:file "raw")))
               (:module "physics"
                :depends-on ("package" "conditions" "helpers" "bounds")
                :components ((:file "toolkit")
                             (:file "constants" :depends-on ("toolkit"))
                             (:file "contact" :depends-on ("core"))
                             (:file "core" :depends-on ("toolkit"))
                             (:file "gjk" :depends-on ("hit-detection" "ray"))
                             (:file "gjk-raycast" :depends-on ("gjk"))
                             (:file "sttb" :depends-on ("gjk-raycast"))
                             (:file "hit-detection" :depends-on ("primitives"))
                             (:file "inertia-tensors" :depends-on ("core"))
                             (:file "particle" :depends-on ("core"))
                             (:file "rigidbody" :depends-on ("primitives" "hit-detection" "ray"))
                             (:file "primitives" :depends-on ("core"))
                             (:file "ray" :depends-on ("primitives"))
                             (:file "resolution" :depends-on ("rigidbody" "contact" "ray"))
                             (:file "trigger" :depends-on ("resolution"))
                             (:file "v-clip" :depends-on ("hit-detection" "ray"))
                             (:file "wave" :depends-on ((:.. "shader-pass") (:.. "effects")))
                             (:file "xenocollide" :depends-on ("hit-detection"))))
               (:module "renderer"
                :depends-on ("package" "shader-pass" "helpers" "gl-struct" "transforms" "animation")
                :components ((:file "cpu-particle" :depends-on ("particle-common"))
                             (:file "gpu-particle" :depends-on ("particle-common"))
                             (:file "lights")
                             (:file "materials" :depends-on ((:.. "assets")))
                             (:file "particle-common" :depends-on ("standard-renderer"))
                             (:file "pbr" :depends-on ("standard-renderer" "shadow-map" "materials"))
                             (:file "phong" :depends-on ("standard-renderer" "shadow-map" "pbr"))
                             (:file "shadow-map" :depends-on ("standard-renderer"))
                             (:file "ssao" :depends-on ("standard-renderer"))
                             (:file "ssr" :depends-on ("standard-renderer" (:.. "effects")))
                             (:file "standard-renderer" :depends-on ("lights" "materials"))
                             (:file "tone-mapping")))
               (:module "resources"
                :depends-on ("package" "resource" "toolkit" "texture-source" "type-info")
                :components ((:file "bindable-buffer" :depends-on ("buffer-object"))
                             (:file "buffer-object")
                             (:file "compute-shader" :depends-on ("shader-program"))
                             (:file "framebuffer")
                             (:file "memory")
                             (:file "shader-program")
                             (:file "shader")
                             (:file "shader-storage-buffer" :depends-on ("bindable-buffer" "struct-buffer"))
                             (:file "struct-buffer" :depends-on ("buffer-object" (:.. "gl-struct")))
                             (:file "texture" :depends-on ((:.. "texture-source")))
                             (:file "uniform-buffer" :depends-on ("bindable-buffer" "struct-buffer"))
                             (:file "vertex-array")
                             (:file "vertex-buffer" :depends-on ("bindable-buffer" "shader-program"))
                             (:file "vertex-struct-buffer" :depends-on ("struct-buffer"))))
               (:module "assets"
                :depends-on ("package" "asset" "resources" "texture-source")
                :components ((:file "audio")
                             (:file "environment-map" :depends-on ("image" "shader-image"))
                             (:file "image" :depends-on ((:.. "texture-source")))
                             (:file "mesh")
                             (:file "model" :depends-on ((:.. "model")))
                             (:file "shader-image" :depends-on ((:.. "shader-entity")))
                             (:file "sprite-data" :depends-on ("image"))
                             (:file "static")
                             (:file "tile-data" :depends-on ("image"))
                             (:file "uniform-block")
                             (:file "video" :depends-on ((:.. "video"))))))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:3d-math
               :3d-spaces
               :alexandria
               :atomics
               :bordeaux-threads
               :cl-gamepad
               :cl-opengl
               :cl-ppcre
               :closer-mop
               :com.inuoe.jzon
               :convex-covering
               :deploy
               :depot
               :depot-zip
               :documentation-utils
               :filesystem-utils
               :float-features
               :flow
               :for
               :form-fiddle
               :glsl-toolkit
               :ieee-floats
               :lambda-fiddle
               :language-codes
               :lquery
               :lru-cache
               :machine-state/opengl
               :manifolds
               :memory-regions
               :messagebox
               :nibbles
               :open-with
               :pathname-utils
               :precise-time
               :promise
               :quickhull
               :random-sampling
               :random-state
               :sha3
               :simple-tasks
               :system-locale
               :text-draw
               :trivial-deprecate
               :trivial-extensible-sequences
               :trivial-garbage
               :trivial-indent
               :verbose
               :zpng
               (:feature :windows :com-on)))

#+sbcl
(when (< (floor (sb-ext:dynamic-space-size) (* 1024 1024 1024)) 2)
  (error "Trial requires at least 2GB of dynamic space size.
SBCL is currently configured for ~dGB

Make sure SBCL is built or run with --dynamic-space-size 2Gb or more."
         (ceiling (sb-ext:dynamic-space-size) (* 1024 1024 1024))))

#+sbcl
(sb-ext:assert-version->= 2 2)
