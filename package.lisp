#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl #:3d-vectors #:3d-matrices #:flare)
  (:shadow #:scene #:entity #:load #:update)
  (:import-from #:static-vectors #:static-vector-pointer)
  (:import-from #:flow #:port)
  ;; assets/font.lisp
  (:export
   #:*default-charset*
   #:font
   #:charset
   #:index
   #:size
   #:oversample
   #:fit-size
   #:text-extent)
  ;; assets/image.lisp
  (:export
   #:image
   #:resize)
  ;; assets/mesh.lisp
  (:export
   #:mesh
   #:geometry-name
   #:attributes
   #:data-usage)
  ;; formats/collada.lisp
  (:export)
  ;; formats/vertex-format.lisp
  (:export)
  ;; resources/framebuffer.lisp
  (:export
   #:framebuffer
   #:attachments
   #:resize)
  ;; resources/shader-program.lisp
  (:export
   #:shader-program
   #:uniform-map
   #:shaders
   #:uniform)
  ;; resources/shader.lisp
  (:export
   #:shader
   #:shader-type
   #:shader-source)
  ;; resources/texture.lisp
  (:export
   #:texture
   #:width
   #:height
   #:depth
   #:target
   #:level
   #:samples
   #:internal-format
   #:pixel-format
   #:pixel-type
   #:pixel-data
   #:mag-filter
   #:min-filter
   #:anisotropy
   #:wrapping
   #:storage
   #:allocate-texture-storage
   #:resize)
  ;; resources/vertex-array.lisp
  (:export
   #:vertex-array
   #:size
   #:bindings)
  ;; resources/vertex-buffer.lisp
  (:export
   #:vertex-buffer
   #:buffer-type
   #:buffer-data
   #:element-type
   #:data-usage
   #:size
   #:update-buffer-data)
  ;; array-container.lisp
  (:export
   #:array-container
   #:objects)
  ;; asset-pool.lisp
  (:export
   #:find-pool
   #:remove-pool
   #:list-pools
   #:pool
   #:name
   #:base
   #:assets
   #:define-pool
   #:asset
   #:list-assets
   #:pool-path
   #:trial)
  ;; asset.lisp
  (:export
   #:asset
   #:pool
   #:name
   #:input
   #:load
   #:reload
   #:coerce-asset-input
   #:define-asset
   #:gl-asset)
  ;; attributes.lisp
  (:export
   #:enable
   #:disable
   #:push-attribs
   #:pop-attribs
   #:with-pushed-attribs)
  ;; camera.lisp
  (:export
   #:camera
   #:near-plane
   #:far-plane
   #:project-view
   #:setup-perspective
   #:2d-camera
   #:slidescroll-camera
   #:zoom
   #:target
   #:3d-camera
   #:fov
   #:target-camera
   #:target
   #:up
   #:pivot-camera
   #:following-camera
   #:fps-camera
   #:rotation
   #:acceleration
   #:x-inverted
   #:y-inverted
   #:freeroam-camera
   #:move-speed
   #:editor-camera)
  ;; context.lisp
  (:export
   #:*context*
   #:with-context
   #:launch-with-context
   #:make-context
   #:context
   #:current-thread
   #:context-waiting
   #:context-lock
   #:context-wait-lock
   #:resources
   #:handler
   #:shared-with
   #:create-context
   #:destroy-context
   #:valid-p
   #:make-current
   #:current-p
   #:done-current
   #:hide
   #:show
   #:resize
   #:quit
   #:swap-buffers
   #:show-cursor
   #:hide-cursor
   #:lock-cursor
   #:unlock-cursor
   #:title
   #:width
   #:height
   #:profile
   #:version
   #:resize
   #:gain-focus
   #:lose-focus
   #:context-info)
  ;; controller.lisp
  (:export
   #:system-action
   #:save-game
   #:load-game
   #:reload-scene
   #:quit-game
   #:toggle-overlay
   #:noto-sans
   #:noto-mono
   #:controller
   #:display
   #:text
   #:show-overlay
   #:load-request
   #:maybe-reload-scene)
  ;; deploy.lisp
  (:export)
  ;; display.lisp
  (:export
   #:display
   #:context
   #:clear-color
   #:setup-rendering
   #:render)
  ;; effects.lisp
  (:export
   #:effects
   #:render-pass
   #:color
   #:depth
   #:msaa-pass
   #:simple-post-effect-pass
   #:previous-pass
   #:color
   #:copy-pass
   #:negative-pass
   #:grayscale-pass
   #:box-blur-pass
   #:sobel-pass
   #:gaussian-blur-pass
   #:radial-blur-pass
   #:fxaa-pass
   #:blend-pass
   #:a-pass
   #:b-pass
   #:color
   #:high-pass-filter
   #:low-pass-filter
   #:chromatic-aberration-filter
   #:black-render-pass
   #:light-scatter-pass
   #:previous-pass
   #:black-render-pass
   #:color)
  ;; entity.lisp
  (:export
   #:matches
   #:entity)
  ;; event-loop.lisp
  (:export
   #:add-handler
   #:remove-handler
   #:handle
   #:handler-container
   #:handlers
   #:priority
   #:event-loop
   #:issue
   #:process
   #:discard-events
   #:handler
   #:event-type
   #:delivery-function
   #:priority
   #:event
   #:tick
   #:tt
   #:dt)
  ;; features.lisp
  (:export
   #:*debug-features*
   #:*optimize-features*
   #:reload-with-features)
  ;; flare.lisp
  (:export
   #:paint
   #:paint-with
   #:finalize)
  ;; fullscreenable.lisp
  (:export
   #:fullscreenable
   #:original-mode
   #:resolution
   #:fullscreen)
  ;; gamepad.lisp
  (:export)
  ;; geometry-clipmap.lisp
  (:export)
  ;; geometry-shapes.lisp
  (:export
   #:make-rectangle
   #:make-cube
   #:make-quad-grid
   #:make-line-grid
   #:make-sphere
   #:make-disc
   #:make-cylinder
   #:make-cone
   #:make-tube)
  ;; geometry.lisp
  (:export
   #:geometry
   #:meshes
   #:read-geometry
   #:write-geometry
   #:sphere-mesh
   #:size
   #:vertex-mesh
   #:face-length
   #:vertex-type
   #:faces
   #:vertices
   #:add-vertex
   #:triangulate
   #:check-mesh-valid
   #:pack
   #:with-vertex-filling
   #:vertex
   #:location
   #:fill-vector-data
   #:vertex-attribute-size
   #:fill-vertex-attribute
   #:vertex-attributes
   #:vertex=
   #:textured-vertex
   #:uv
   #:normal-vertex
   #:normal
   #:colored-vertex
   #:color
   #:basic-vertex)
  ;; helpers.lisp
  (:export
   #:located-entity
   #:location
   #:oriented-entity
   #:orientation
   #:up
   #:rotated-entity
   #:rotation
   #:axis-rotated-entity
   #:axis
   #:angle
   #:pivoted-entity
   #:pivot
   #:clocked-subject
   #:vertex-entity
   #:colored-entity
   #:vertex-colored-entity
   #:textured-entity
   #:texture)
  ;; input.lisp
  (:export
   #:input-event
   #:keyboard-event
   #:key-event
   #:key
   #:modifiers
   #:key-press
   #:key-release
   #:text-entered
   #:mouse-event
   #:mouse-button-event
   #:mouse-press
   #:mouse-release
   #:mouse-scroll
   #:delta
   #:mouse-move
   #:old-pos
   #:gamepad-event
   #:device
   #:gamepad-attach
   #:gamepad-remove
   #:gamepad-press
   #:button
   #:gamepad-release
   #:button
   #:gamepad-move
   #:axis
   #:old-pos
   #:pos
   #:key
   #:mouse
   #:gamepad)
  ;; layer-set.lisp
  (:export
   #:layer-container
   #:layer
   #:active
   #:layer-set
   #:objects
   #:index-map
   #:layered-unit
   #:layer)
  ;; loader.lisp
  (:export
   #:banned-slots
   #:compute-resources
   #:resources-ready
   #:bake
   #:baked-p
   #:transition
   #:dependencies
   #:bakable)
  ;; main.lisp
  (:export
   #:main
   #:scene
   #:controller
   #:setup-scene
   #:change-scene
   #:launch)
  ;; mapping.lisp
  (:export
   #:mapping
   #:remove-mapping
   #:define-mapping
   #:define-simple-mapping
   #:map-event
   #:action
   #:remove-action-mappings
   #:define-action)
  ;; phong.lisp
  (:export)
  ;; pipeline.lisp
  (:export
   #:pipeline
   #:nodes
   #:passes
   #:textures
   #:clear-pipeline
   #:connect
   #:check-consistent
   #:resize
   #:pack-pipeline)
  ;; pipelined-scene.lisp
  (:export
   #:pipelined-scene)
  ;; rails.lisp
  (:export
   #:rail
   #:target
   #:rail-points
   #:duration
   #:rail-location
   #:linear-rail
   #:rail-times)
  ;; redefinition-notifying-class.lisp
  (:export
   #:redefinition-notifying-class
   #:class-redefinition-listeners
   #:add-class-redefinition-listener
   #:remove-class-redefinition-listener)
  ;; render-texture.lisp
  (:export
   #:render-texture
   #:width
   #:height
   #:clear-color
   #:texture)
  ;; renderable.lisp
  (:export
   #:renderable
   #:thread
   #:delta-time
   #:frame-time
   #:start
   #:stop
   #:render
   #:update)
  ;; resource.lisp
  (:export
   #:resource
   #:allocate
   #:deallocate
   #:allocated-p
   #:check-allocated
   #:foreign-resource
   #:data-pointer
   #:destructor
   #:gl-resource
   #:gl-name)
  ;; retention.lisp
  (:export
   #:retained
   #:clear-retained
   #:retention-function
   #:remove-retention-function
   #:retain-event
   #:define-retention
   #:define-coupled-retention
   #:define-uniform-retention)
  ;; scene-buffer.lisp
  (:export
   #:scene-buffer
   #:render-pass)
  ;; scene.lisp
  (:export
   #:*scene*
   #:scene
   #:scene-event
   #:scene
   #:enter
   #:entity
   #:leave
   #:register
   #:deregister
   #:paint
   #:process)
  ;; sdl2-gamepad-map.lisp
  (:export)
  ;; selection-buffer.lisp
  (:export
   #:ensure-selection-color
   #:selection-buffer
   #:scene
   #:color->object-map
   #:object-at-point
   #:color->object
   #:selection-buffer-pass
   #:selectable
   #:selection-color
   #:find-new-selection-color)
  ;; shader-entity.lisp
  (:export
   #:shader-entity-class
   #:effective-shaders
   #:direct-shaders
   #:inhibited-shaders
   #:compute-effective-shaders
   #:class-shader
   #:remove-class-shader
   #:make-class-shader-program
   #:define-class-shader
   #:shader-entity
   #:define-shader-entity
   #:determine-effective-shader-class)
  ;; shader-pass.lisp
  (:export
   #:shader-pass-class
   #:texture-port
   #:texture
   #:texspec
   #:uniform-port
   #:uniform-name
   #:input
   #:output
   #:attachment
   #:check-consistent
   #:buffer
   #:shader-pass
   #:framebuffer
   #:uniforms
   #:register-object-for-pass
   #:shader-program-for-pass
   #:make-pass-shader-program
   #:coerce-pass-shader
   #:define-shader-pass
   #:generate-pass-program
   #:prepare-pass-program
   #:per-object-pass
   #:assets
   #:notify-class-redefinition
   #:single-shader-pass
   #:shader-program
   #:post-effect-pass)
  ;; shader-subject.lisp
  (:export
   #:shader-subject-class
   #:shader-subject
   #:define-shader-subject)
  ;; skybox.lisp
  (:export
   #:skybox
   #:texture
   #:vertex-array)
  ;; sprite.lisp
  (:export
   #:sprite-entity
   #:tile
   #:size
   #:animated-sprite-subject
   #:animations
   #:clock
   #:frame
   #:animation
   #:update-sprite-animation)
  ;; static-vector.lisp
  (:export
   #:make-static-vector
   #:static-vector-p
   #:static-vector
   #:maybe-free-static-vector)
  ;; subject.lisp
  (:export
   #:subject-class
   #:effective-handlers
   #:compute-effective-handlers
   #:subject
   #:event-loops
   #:regenerate-handlers
   #:define-subject
   #:subject-handler
   #:subject
   #:define-handler
   #:define-generic-handler)
  ;; text.lisp
  (:export
   #:text
   #:font
   #:text
   #:size
   #:extent
   #:text-extent
   #:highlighted-text
   #:color-regions)
  ;; toolkit.lisp
  (:export
   #:finalize
   #:gl-property
   #:with-float-traps-masked
   #:current-time
   #:executable-directory
   #:enlist
   #:unlist
   #:remf*
   #:one-of
   #:input-source
   #:input-value
   #:input-literal
   #:with-retry-restart
   #:with-new-value-restart
   #:with-cleanup-on-failure
   #:acquire-lock-with-starvation-test
   #:*standalone*
   #:standalone-error-handler
   #:standalone-logging-handler
   #:make-thread
   #:with-thread
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-timing-report
   #:ensure-class
   #:with-slots-bound
   #:with-all-slots-bound
   #:minimize
   #:def->rad
   #:rad->deg
   #:symbol->c-name
   #:check-gl-type
   #:gl-type-size
   #:cl-type->gl-type
   #:gl-type->cl-type
   #:gl-coerce
   #:texture-internal-format->pixel-format
   #:pixel-format->pixel-type
   #:gpu-room
   #:cpu-room
   #:gl-vendor
   #:check-texture-size
   #:define-enum-check
   #:check-texture-target
   #:check-texture-mag-filter
   #:check-texture-min-filter
   #:check-texture-wrapping
   #:check-texture-internal-format
   #:check-texture-pixel-format
   #:check-texture-pixel-type
   #:check-shader-type
   #:check-vertex-buffer-type
   #:check-vertex-buffer-element-type
   #:check-vertex-buffer-data-usage
   #:check-framebuffer-attachment)
  ;; transforms.lisp
  (:export
   #:*view-matrix*
   #:*projection-matrix*
   #:*model-matrix*
   #:view-matrix
   #:projection-matrix
   #:model-matrix
   #:look-at
   #:perspective-projection
   #:orthographic-projection
   #:with-pushed-matrix
   #:translate
   #:translate-by
   #:rotate
   #:rotate-by
   #:scale
   #:scale-by
   #:reset-matrix
   #:vec->screen
   #:screen->vec)
  ;; window.lisp
  (:export
   #:window
   #:register-window
   #:deregister-window
   #:list-windows
   #:window
   #:name)
  ;; workbench.lisp
  (:export))

(defpackage #:cl+trial
  (:nicknames #:org.shirakumo.fraf.trial.cl+trial)
  (:shadowing-import-from #:trial #:scene #:entity #:load #:update)
  (:use #:cl #:trial #:3d-vectors #:flare))

(do-symbols (symb '#:cl+trial)
  (export (list symb) '#:cl+trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+trial))
