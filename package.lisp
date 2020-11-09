#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl #:3d-vectors #:3d-matrices #:flare)
  (:shadow #:// #:scene #:entity #:container #:load #:update #:particle)
  (:import-from #:static-vectors #:static-vector-pointer)
  (:import-from #:flow #:port)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad))
  ;; assets/font.lisp
  (:export
   #:font)
  ;; assets/image.lisp
  (:export
   #:image-loader
   #:image
   #:resize)
  ;; assets/mesh.lisp
  (:export
   #:mesh-loader
   #:mesh
   #:geometry-name
   #:attributes
   #:data-usage)
  ;; assets/sprite-data.lisp
  (:export
   #:sprite-data
   #:vertex-array
   #:texture
   #:animations
   #:frames
   #:load-animations)
  ;; assets/static.lisp
  (:export
   #:static)
  ;; assets/uniform-block.lisp
  (:export
   #:uniform-block)
  ;; formats/collada.lisp
  (:export)
  ;; formats/vertex-format.lisp
  (:export)
  ;; resources/buffer-object.lisp
  (:export
   #:buffer-object
   #:buffer-type
   #:buffer-data
   #:data-usage
   #:size
   #:update-buffer-data
   #:resize-buffer)
  ;; resources/font.lisp
  (:export
   #:*default-charset*
   #:font-atlas
   #:charset
   #:index
   #:size
   #:oversample
   #:fit-size
   #:text-extent)
  ;; resources/framebuffer.lisp
  (:export
   #:framebuffer
   #:attachments
   #:resize
   #:capture
   #:blit-to-screen)
  ;; resources/shader-program.lisp
  (:export
   #:shader-program
   #:uniform-map
   #:shaders
   #:uniform
   #:uniform-location
   #:uniforms)
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
  ;; resources/uniform-buffer.lisp
  (:export
   #:uniform-buffer
   #:qualifiers
   #:binding
   #:struct
   #:with-buffer-tx)
  ;; resources/vertex-array.lisp
  (:export
   #:vertex-array
   #:vertex-form
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
   #:placeholder-resource
   #:asset
   #:pool
   #:name
   #:input
   #:loaded-p
   #:load
   #:reload
   #:unload
   #:list-resources
   #://
   #:coerce-asset-input
   #:input*
   #:define-asset
   #:single-resource-asset
   #:multi-resource-asset
   #:file-input-asset)
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
   #:vsync
   #:width
   #:height
   #:profile
   #:version
   #:resize
   #:gain-focus
   #:lose-focus
   #:window-hidden
   #:window-shown
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
   #:observe
   #:observe!
   #:stop-observing
   #:load-request
   #:maybe-reload-scene
   #:display-controller)
  ;; deferred.lisp
  (:export
   #:geometry-pass
   #:depth
   #:position
   #:normal
   #:albedo
   #:metal
   #:geometry-shaded
   #:diffuse-map
   #:specular-map
   #:normal-map
   #:roughness-map
   #:occlusion-map
   #:deferred-render-pass
   #:position-map
   #:normal-map
   #:albedo-map
   #:metal-map
   #:light
   #:light-block)
  ;; deploy.lisp
  (:export)
  ;; display.lisp
  (:export
   #:display
   #:poll-input
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
   #:color
   #:visualizer-pass
   #:t[0] #:t[1] #:t[2] #:t[3])
  ;; entity.lisp
  (:export
   #:entity
   #:container)
  ;; event-loop.lisp
  (:export
   #:event
   #:listener
   #:add-listener
   #:remove-listener
   #:handle
   #:event-loop
   #:issue
   #:process
   #:discard-events
   #:define-handler
   #:tick
   #:tt
   #:dt
   #:fc)
  ;; features.lisp
  (:export
   #:*debug-features*
   #:*optimize-features*
   #:reload-with-features)
  ;; flare.lisp
  (:export
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
   #:make-triangle
   #:make-cube
   #:make-quad-grid
   #:make-line-grid
   #:make-sphere
   #:make-disc
   #:make-cylinder
   #:make-cone
   #:make-tube
   #:make-lines
   #:fullscreen-square
   #:empty-vertex-array
   #:axes
   #:2d-axes)
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
   #:basic-vertex
   #:replace-vertex-data
   #:make-vertex-data)
  ;; gl-struct.lisp
  (:export
   #:gl-struct
   #:storage-ptr
   #:compute-depedent-types
   #:gl-source
   #:gl-struct-class
   #:gl-type
   #:layout-standard
   #:struct-fields
   #:define-gl-struct
   #:gl-vector
   #:element-type)
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
   #:scaled-entity
   #:scaling
   #:clocked-entity
   #:vertex-entity
   #:colored-entity
   #:vertex-colored-entity
   #:textured-entity
   #:texture)
  ;; hdr.lisp
  (:export
   #:hdr-output-pass
   #:tone-mapping-pass
   #:high-color-pass
   #:bloom-pass
   #:high-pass)
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
  ;; layered-container.lisp
  (:export
   #:layered-container
   #:layer-index
   #:layer-count)
  ;; loader.lisp
  (:export
   #:staging-area
   #:staged
   #:dependencies
   #:stage
   #:unstage
   #:compute-load-sequence
   #:loader
   #:commit
   #:abort-commit
   #:load-with
   #:unload-with
   #:progress)
  ;; main.lisp
  (:export
   #:main
   #:scene
   #:controller
   #:setup-scene
   #:change-scene
   #:enter-and-load
   #:launch)
  ;; mapping.lisp
  (:export
   #:retained
   #:clear-retained
   #:mapping
   #:remove-mapping
   #:define-mapping
   #:define-simple-mapping
   #:map-event
   #:action
   #:remove-action-mappings
   #:define-action
   #:load-mapping
   #:trigger
   #:retain)
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
  ;; prompt.lisp
  (:export
   #:prompt-char
   #:prompt-charset
   #:prompt-font
   #:prompt
   #:prompt-icon)
  ;; rails.lisp
  (:export
   #:rail
   #:target
   #:rail-points
   #:duration
   #:rail-location
   #:linear-rail
   #:rail-times)
  ;; render-texture.lisp
  (:export
   #:render-texture
   #:width
   #:height
   #:clear-color
   #:texture)
  ;; render-loop.lisp
  (:export
   #:render-loop
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
   #:generator
   #:name
   #:allocate
   #:deallocate
   #:allocated-p
   #:check-allocated
   #:foreign-resource
   #:data-pointer
   #:gl-resource
   #:gl-name)
  ;; resource-generator.lisp
  (:export
   #:resource-generator
   #:generate-resources
   #:register-generation-observer
   #:clear-observers
   #:observe-generation
   #:resource)
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
   #:effective-buffers
   #:direct-shaders
   #:direct-buffers
   #:inhibited-shaders
   #:compute-effective-shaders
   #:class-shader
   #:remove-class-shader
   #:make-class-shader-program
   #:define-class-shader
   #:shader-entity
   #:define-shader-entity
   #:effective-shader-class
   #:compute-effective-shader-class
   #:standalone-shader-entity
   #:shader-program)
  ;; shader-pass.lisp
  (:export
   #:port
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
   #:active-p
   #:renderable
   #:transformed
   #:apply-transforms
   #:object-renderable-p
   #:compile-to-pass
   #:compile-into-pass
   #:remove-from-pass
   #:register-object-for-pass
   #:shader-program-for-pass
   #:make-pass-shader-program
   #:coerce-pass-shader
   #:define-shader-pass
   #:generate-pass-program
   #:prepare-pass-program
   #:scene-pass
   #:per-object-pass
   #:assets
   #:single-shader-pass
   #:single-shader-scene-pass
   #:shader-program
   #:post-effect-pass)
  ;; shadow-map.lisp
  (:export
   #:shadow-map-pass
   #:shadow
   #:shadow-projection-matrix
   #:shadow-view-matrix
   #:shadow-render-pass
   #:shadow-map
   #:shadow-map-pass)
  ;; skybox.lisp
  (:export
   #:skybox
   #:texture
   #:vertex-array
   #:skybox-pass)
  ;; sprite.lisp
  (:export
   #:sprite-frame
   #:xy
   #:uv
   #:duration
   #:sprite-animation
   #:name
   #:start
   #:end
   #:next-animation
   #:loop-to
   #:sprite-entity
   #:frame-idx
   #:frames
   #:make-sprite-frame-mesh
   #:frame
   #:animated-sprite
   #:clock
   #:animations
   #:animation
   #:playback-speed
   #:playback-direction
   #:reset-animation
   #:switch-animation)
  ;; ssao,lisp
  (:export
   #:ssao-pass
   #:position-map
   #:normal-map
   #:occlusion)
  ;; static-vector.lisp
  (:export
   #:make-static-vector
   #:static-vector-p
   #:static-vector
   #:maybe-free-static-vector)
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
   #:define-global
   #:finalize
   #:gl-property
   #:current-time
   #:executable-directory
   #:kw
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
   #:tempdir
   #:tempfile
   #:logfile
   #:config-directory
   #:standalone-error-handler
   #:standalone-logging-handler
   #:make-thread
   #:with-thread
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-timing-report
   #:ensure-class
   #:ensure-instance
   #:with-slots-bound
   #:with-all-slots-bound
   #:minimize
   #:clamp
   #:lerp
   #:damp*
   #:deg->rad
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
  (:shadowing-import-from #:trial #:// #:scene #:entity #:container #:load #:update)
  (:use #:cl #:trial #:3d-vectors #:3d-matrices #:flare))

(do-symbols (symb '#:cl+trial)
  (export (list symb) '#:cl+trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+trial))
