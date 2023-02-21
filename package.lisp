#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl
        #:org.shirakumo.flare.vector
        #:org.shirakumo.flare.matrix
        #:org.shirakumo.flare.quaternion
        #:org.shirakumo.flare.transform)
  (:shadow #:// #:load)
  (:import-from #:static-vectors #:static-vector-pointer)
  (:import-from #:flow #:port)
  (:local-nicknames
   (#:gamepad #:org.shirakumo.fraf.gamepad)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:v #:org.shirakumo.verbose)
   (#:promise #:org.shirakumo.promise)
   #+windows
   (#:com #:org.shirakumo.com-on))
  ;; animation/asset.lisp
  (:export
   #:animation-asset
   #:meshes
   #:clips
   #:skeleton)
  ;; animation/clip.lisp
  (:export
   #:clip
   #:tracks
   #:start-time
   #:end-time
   #:loop-p
   #:duration
   #:find-animation-track)
  ;; animation/entity.lisp
  (:export
   #:animation-layer
   #:strength
   #:layer-controller
   #:layers
   #:add-layer
   #:remove-layer
   #:layer
   #:fade-controller
   #:play
   #:fade-to
   #:armature
   #:animation-asset
   #:animated-entity
   #:mesh
   #:animation-asset)
  ;; animation/mesh.lisp
  (:export
   #:skinned-mesh
   #:position-normals
   #:vertex-data
   #:index-data
   #:skinned-p
   #:cpu-skin)
  ;; animation/pose.lisp
  (:export
   #:pose
   #:joints
   #:parents
   #:pose<-
   #:pose=
   #:parent-joint
   #:global-transform
   #:matrix-palette
   #:blend-into
   #:layer-onto)
  ;; animation/skeleton.lisp
  (:export
   #:skeleton
   #:instantiate-clip
   #:rest-pose
   #:bind-pose
   #:inv-bind-pose
   #:rest-pose*)
  ;; animation/track.lisp
  (:export
   #:animation-frame
   #:animation-track
   #:frames
   #:interpolation
   #:start-time
   #:end-time
   #:duration
   #:sample
   #:find-frame-idx
   #:valid-p
   #:fast-animation-track
   #:transform-track
   #:location
   #:scaling
   #:rotation)
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
  ;; assets/tile-data.lisp
  (:export
   #:tileset
   #:tile-size
   #:tilemap
   #:tileset
   #:tile-data)
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
  ;; resources/vertex-struct-buffer.lisp
  (:export
   #:vertex-struct-buffer)
  ;; achievements.lisp
  (:export
   #:achievement
   #:list-achievements
   #:name
   #:title
   #:description
   #:icon
   #:event-type
   #:test-function
   #:active-p
   #:define-achievement
   #:award
   #:achievement-event
   #:achievement-unlocked
   #:achievement-relocked
   #:+achievement-api+
   #:*achievement-apis*
   #:achievement-api
   #:load-achievement-data
   #:save-achievement-data
   #:notifications-display-p
   #:local-achievement-api)
  ;; actions.lisp
  (:export
   #:action-set
   #:exclusive-action-set
   #:active-p
   #:find-action-set
   #:list-action-sets
   #:active-action-set
   #:define-action-set
   #:action
   #:source-event
   #:analog-action
   #:value
   #:directional-action
   #:x
   #:y
   #:spatial-action
   #:x
   #:y
   #:z
   #:define-action)
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
   #:generate-assets-from-path
   #:define-assets-from-path
   #:single-resource-asset
   #:multi-resource-asset
   #:file-input-asset)
  ;; async.lisp
  (:export
   #:task-thread
   #:start
   #:stop
   #:task-runner-main
   #:promise-task
   #:with-eval-in-task-thread)
  ;; attributes.lisp
  (:export
   #:enable
   #:disable
   #:push-attribs
   #:pop-attribs
   #:with-pushed-attribs)
  ;; bag.lisp
  (:export
   #:bag)
  ;; camera.lisp
  (:export
   #:camera
   #:near-plane
   #:far-plane
   #:project-view
   #:setup-perspective
   #:map-visible
   #:in-view-p
   #:do-visible
   #:2d-camera
   #:sidescroll-camera
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
  ;; capture.lisp
  (:export
   #:start
   #:stop
   #:active-p
   #:capture
   #:start-capture
   #:stop-capture
   #:replay
   #:start-replay
   #:stop-replay)
  ;; conditions.lisp
  (:export
   #:trial-error
   #:thread-did-not-exit
   #:resource-not-allocated
   #:context-creation-error
   #:resource-depended-on
   #:shader-compilation-error
   #:initarg-not-supplied
   #:arg!)
  ;; container.lisp
  (:export
   #:scene-node
   #:container
   #:scene
   #:clear
   #:enter
   #:leave
   #:register
   #:deregister
   #:contains-p
   #:map-scene-graph
   #:do-scene-graph
   #:entity
   #:name)
  ;; context.lisp
  (:export
   #:*context*
   #:context-creation-error
   #:with-context
   #:launch-with-context
   #:make-context
   #:monitor
   #:name
   #:context
   #:current-thread
   #:context-waiting
   #:context-lock
   #:context-wait-lock
   #:resources
   #:handler
   #:shared-with
   #:glsl-target-version
   #:create-context
   #:destroy-context
   #:valid-p
   #:make-current
   #:current-p
   #:done-current
   #:hide
   #:show
   #:visible-p
   #:resize
   #:quit
   #:swap-buffers
   #:show-cursor
   #:hide-cursor
   #:lock-cursor
   #:unlock-cursor
   #:cursor
   #:title
   #:vsync
   #:width
   #:height
   #:profile
   #:version
   #:resize
   #:list-video-modes
   #:current-monitor
   #:list-monitors
   #:find-monitor
   #:clipboard
   #:cursor-position
   #:local-key-string
   #:gain-focus
   #:lose-focus
   #:window-hidden
   #:window-shown
   #:window-close
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
   #:eval-request
   #:func
   #:call-in-render-loop
   #:with-eval-in-render-loop
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
   #:iterative-post-effect-pass
   #:iterations
   #:temporal-post-effect-pass
   #:previous
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
   #:container
   #:clear)
  ;; error-handling.lisp
  (:export
   #:*error-report-hook*
   #:*inhibit-standalone-error-handler*
   #:standard-error-hook
   #:emessage
   #:report-on-error
   #:standalone-error-handler)
  ;; event-loop.lisp
  (:export
   #:event
   #:listener
   #:add-listener
   #:remove-listener
   #:handle
   #:make-event
   #:event-loop
   #:issue
   #:process
   #:discard-events
   #:define-handler
   #:undefine-handler
   #:define-event
   #:define-event-pool
   #:tick
   #:tt
   #:dt
   #:fc
   #:class-changed
   #:changed-class)
  ;; features.lisp
  (:export
   #:*debug-features*
   #:*optimize-features*
   #:reload-with-features)
  ;; fps.lisp
  (:export
   #:fps-counter)
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
  ;; hash-table-container.lisp
  (:export
   #:hash-table-container)
  ;; helpers.lisp
  (:export
   #:located-entity
   #:location
   #:sized-entity
   #:bsize
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
   #:transformed-entity
   #:tf
   #:fullscreen-entity
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
   #:+input-source+
   #:input-event
   #:keyboard-event
   #:key-event
   #:key
   #:modifiers
   #:repeat-p
   #:key-press
   #:key-release
   #:text-entered
   #:replace-p
   #:mouse-event
   #:mouse-button-event
   #:mouse-press
   #:mouse-release
   #:mouse-double-click
   #:mouse-scroll
   #:delta
   #:mouse-move
   #:old-pos
   #:file-drop-event
   #:paths
   #:gamepad-event
   #:device
   #:gamepad-attach
   #:gamepad-remove
   #:gamepad-button-event
   #:button
   #:gamepad-press
   #:gamepad-release
   #:gamepad-move
   #:axis
   #:old-pos
   #:pos
   #:key
   #:mouse
   #:gamepad
   #:gamepad-added
   #:gamepad-removed)
  ;; interpolation.lisp
  (:export
   #:bezier
   #:hermite
   #:linear
   #:constant
   #:interpolate
   #:ninterpolate)
  ;; language.lisp
  (:export
   #:language
   #:languages
   #:language-dir
   #:language-files
   #:define-language-change-hook
   #:load-language
   #:save-language
   #:language-string
   #:ensure-language-string
   #:@format
   #:@formats
   #:@)
  ;; layered-container.lisp
  (:export
   #:layered-container
   #:layer-index
   #:layer-count)
  ;; lines.lisp
  (:export
   #:lines
   #:line-width)
  ;; list-container.lisp
  (:export
   #:list-container)
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
   #:+main+
   #:main
   #:username
   #:scene
   #:controller
   #:setup-scene
   #:change-scene
   #:enter-and-load
   #:launch)
  ;; mapping.lisp
  (:export
   #:+map-key-events+
   #:trigger
   #:keymap
   #:mapping-function
   #:remove-mapping-function
   #:map-event
   #:retained
   #:clear-retained
   #:reset-retained
   #:action-mapping
   #:action-type
   #:event-type
   #:qualifier
   #:mapping-active-p
   #:active-mappings
   #:event-applicable-p
   #:event-active-p
   #:perform-event-mapping
   #:from-mapping-description
   #:to-mapping-description
   #:event-from-action-mapping
   #:event-to-action-mapping
   #:stratify-action-mapping
   #:digital-mapping
   #:threshold
   #:toggle-p
   #:compile-mapping
   #:load-mapping
   #:save-mapping
   #:find-action-mappings
   #:update-action-mappings)
  ;; os-resources.lisp
  (:export
   #:cpu-time
   #:io-bytes
   #:gpu-room
   #:cpu-room)
  ;; parallax.lisp
  (:export
   #:parallax
   #:parallax-background
   #:change-background)
  ;; particle.lisp
  (:export
   #:particle-system
   #:particle-capacity
   #:active-particles
   #:lifetime
   #:clock)
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
   #:prompt-string
   #:action-strings
   #:action-string)
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
   #:target-frame-time
   #:reset-render-loop
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
   #:resource
   #:compiled-generator
   #:compile-resources
   #:recompile-needed-p)
  ;; scene-buffer.lisp
  (:export
   #:scene-buffer
   #:render-pass)
  ;; scene.lisp
  (:export
   #:unit
   #:node
   #:scene
   #:name-map)
  ;; serialize.lisp
  (:export
   #:define-type-serializer
   #:define-object-type-serializer
   #:serialize-as
   #:deserialize-as)
  ;; settings.lisp
  (:export
   #:+settings+
   #:setting-file-path
   #:keymap-path
   #:load-keymap
   #:save-keymap
   #:load-settings
   #:save-settings
   #:setting
   #:observe-setting
   #:remove-setting-observer
   #:define-setting-observer
   #:video-mode
   #:fullscreen
   #:vsync
   #:framerate
   #:fps-counter)
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
   #:fixed-input
   #:static-input
   #:check-consistent
   #:buffer
   #:shader-pass
   #:framebuffer
   #:active-p
   #:renderable
   #:transformed
   #:renderable
   #:dynamic-renderable
   #:apply-transforms
   #:bind-textures
   #:object-renderable-p
   #:shader-program-for-pass
   #:make-pass-shader-program
   #:coerce-pass-shader
   #:define-shader-pass
   #:generate-pass-program
   #:prepare-pass-program
   #:scene-pass
   #:per-object-pass
   #:construct-frame
   #:render-frame
   #:sort-frame
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
   #:find-animation
   #:animation
   #:playback-speed
   #:playback-direction
   #:reset-animation
   #:switch-animation
   #:play)
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
   #:debug-text
   #:text
   #:foreground
   #:background)
  ;; tile-layer.lisp
  (:export
   #:tile-layer
   #:tileset
   #:tilemap
   #:tile-size
   #:visibility
   #:resize
   #:size
   #:tile
   #:clear)
  ;; toolkit.lisp
  (:export
   #:define-global
   #:+app-vendor+
   #:+app-system+
   #:data-root
   #:git-repo-commit
   #:version
   #:toolkit
   #:finalize
   #:gl-property
   #:current-time
   #:open-in-browser
   #:open-in-file-manager
   #:kw
   #:enlist
   #:unlist
   #:remf*
   #:popf
   #:one-of
   #:input-source
   #:input-value
   #:input-literal
   #:with-retry-restart
   #:with-new-value-restart
   #:with-unwind-protection
   #:with-cleanup-on-failure
   #:acquire-lock-with-starvation-test
   #:with-trial-io-syntax
   #:tempdir
   #:tempfile
   #:with-tempfile
   #:rename-file*
   #:make-uuid
   #:logfile
   #:config-directory
   #:standalone-logging-handler
   #:rename-thread
   #:make-thread
   #:with-thread
   #:thread-did-not-exit
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-ignored-errors-on-release
   #:with-timing-report
   #:ensure-class
   #:list-subclasses
   #:list-leaf-classes
   #:format-timestring
   #:descriptor
   #:ensure-instance
   #:type-prototype
   #:maybe-finalize-inheritance
   #:with-slots-bound
   #:with-all-slots-bound
   #:initargs
   #:clone
   #:minimize
   #:generate-name
   #:clamp
   #:deadzone
   #:lerp
   #:deg->rad
   #:rad->deg
   #:db
   #:angle-midpoint
   #:symbol->c-name
   #:c-name->symbol
   #:check-gl-type
   #:gl-type-size
   #:cl-type->gl-type
   #:gl-type->cl-type
   #:gl-coerce
   #:texture-internal-format->pixel-format
   #:pixel-format->pixel-type
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
   #:check-framebuffer-attachment
   #:when-gl-extension)
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

(defpackage #:org.shirakumo.fraf.trial.bvh2
  (:use #:cl #:3d-vectors)
  (:import-from #:org.shirakumo.fraf.trial #:location #:bsize)
  (:export
   #:bvh
   #:make-bvh
   #:bvh-insert
   #:bvh-remove
   #:bvh-update
   #:bvh-check
   #:bvh-print
   #:bvh-lines
   #:bvh-reinsert-all
   #:call-with-contained
   #:call-with-overlapping
   #:do-fitting))

(defpackage #:cl+trial
  (:nicknames #:org.shirakumo.fraf.trial.cl+trial)
  (:shadowing-import-from #:trial #:// #:load)
  (:use #:cl
        #:trial
        #:org.shirakumo.flare.vector
        #:org.shirakumo.flare.matrix
        #:org.shirakumo.flare.quaternion
        #:org.shirakumo.flare.transform))

(let ((symbols ()))
  (do-symbols (symb '#:cl+trial) (push symb symbols))
  (export symbols '#:cl+trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+trial))
