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
  ;; asset-pool.lisp
  (:export
   #:pool
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
   #:load-request
   #:asset
   #:action
   #:define-asset)
  ;; asset.lisp
  (:export
   #:load
   #:offload
   #:asset
   #:inputs
   #:resource
   #:finalize-resource
   #:install-finalizer
   #:coerce-input
   #:coerced-inputs
   #:reload
   #:make-asset
   #:with-assets
   #:clear-asset-cache
   #:with-assets*)
  ;; attributes.lisp
  (:export
   #:*gl-attributes*
   #:*default-enabled-gl-attributes*
   #:reset-attributes
   #:attribute-table
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
   #:sidescroll-camera
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
   #:assets
   #:handler
   #:create-context
   #:destroy-context
   #:valid-p
   #:make-current
   #:done-current
   #:hide
   #:show
   #:quit
   #:resize
   #:swap-buffers
   #:show-cursor
   #:hide-cursor
   #:title
   #:width
   #:height
   #:profile
   #:version
   #:acquire-context
   #:release-context
   #:resize
   #:context-info
   #:context-note-debug-info)
  ;; controller.lisp
  (:export
   #:system-action
   #:save-game
   #:load-game
   #:quit-game
   #:reload-assets
   #:reload-scene
   #:controller
   #:display
   #:maybe-reload-scene)
  ;; deploy.lisp
  (:export)
  ;; display.lisp
  (:export
   #:display
   #:context
   #:clear-color
   #:handle
   #:setup-rendering
   #:paint
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
   #:fxaa-pass
   #:blend-pass
   #:high-pass-filter
   #:low-pass-filter
   #:chromatic-aberration-effect)
  ;; entity.lisp
  (:export
   #:matches
   #:entity)
  ;; event-loop.lisp
  (:export
   #:*loop*
   #:add-handler
   #:remove-handler
   #:handle
   #:handler-container
   #:handlers
   #:priority
   #:event-loop
   #:queue
   #:queue-index
   #:issue
   #:process
   #:discard-events
   #:handler
   #:event-type
   #:container
   #:delivery-function
   #:priority
   #:event
   #:tick
   #:clock
   #:delta)
  ;; features.lisp
  (:export
   #:*debug-features*
   #:*optimize-features*
   #:reload-with-features)
  ;; flare.lisp
  (:export)
  ;; fullscreenable.lisp
  (:export
   #:fullscreenable
   #:original-mode
   #:resolution
   #:fullscreen)
  ;; gamepad.lisp
  (:export
   #:add-gamepad-handler
   #:remove-gamepad-handler
   #:define-gamepad
   #:xbox-360
   #:logitech-f310
   #:dualshock-3
   #:buffalo-bsgp801
   #:steam-controller)
  ;; geometry-shapes.lisp
  (:export
   #:geometry
   #:fullscreen-square
   #:make-rectangle
   #:make-cube)
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
   #:padd-vertex
   #:triangulate
   #:check-mesh-valid
   #:pack
   #:with-vertex-filling
   #:vertex
   #:location
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
   #:pivoted-entity
   #:pivot)
  ;; input.lisp
  (:export
   #:input-event
   #:keyboard-event
   #:key
   #:text
   #:modifiers
   #:key-press
   #:key-release
   #:mouse-event
   #:pos
   #:mouse-button-event
   #:button
   #:mouse-press
   #:mouse-release
   #:mouse-scroll
   #:delta
   #:mouse-move
   #:old-pos
   #:gamepad-event
   #:gamepad-attach
   #:gamepad-remove
   #:gamepad-press
   #:gamepad-release
   #:gamepad-move
   #:key
   #:mouse
   #:gamepad)
  ;; layer-set.lisp
  (:export
   #:layer-container
   #:layer
   #:active
   #:layer-set
   #:unit
   #:enter
   #:leave
   #:layer-active-p
   #:layered-unit
   #:layer)
  ;; main.lisp
  (:export
   #:main
   #:scene
   #:pipeline
   #:setup-scene
   #:setup-pipeline
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
  ;; pipeline.lisp
  (:export
   #:pipeline
   #:nodes
   #:passes
   #:textures
   #:register
   #:deregister
   #:clear
   #:connect
   #:check-consistent
   #:pack-pipeline)
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
  ;; scene.lisp
  (:export
   #:scene
   #:scene-event
   #:scene
   #:enter
   #:entity
   #:leave
   #:register
   #:deregister)
  ;; shader-pass.lisp
  (:export
   #:shader-pass-class
   #:port
   #:input
   #:uniform-name
   #:texture
   #:output
   #:attachment
   #:texture
   #:shader-pass
   #:framebuffer
   #:register-object-for-pass
   #:shader-program-for-pass
   #:define-shader-pass
   #:per-object-pass
   #:assets
   #:update-shader-for-redefined-subject
   #:shader-program-for-pass
   #:coerce-pass-shader
   #:multisampled-pass
   #:multisample-fbo
   #:samples
   #:multisampled-per-object-pass
   #:single-shader-pass
   #:shader-program
   #:post-effect-pass
   #:vertex-array)
  ;; shader-subject.lisp
  (:export
   #:shader-subject-class
   #:effective-shaders
   #:direct-shaders
   #:class-shader
   #:remove-class-shader
   #:make-class-shader-program
   #:define-class-shader
   #:shader-subject
   #:define-shader-subject)
  ;; sprite.lisp
  (:export
   #:sprite-subject
   #:tile
   #:size
   #:animated-sprite-subject
   #:animations
   #:frame
   #:animation
   #:clock)
  ;; static-vector.lisp
  (:export
   #:make-static-vector
   #:static-vector-p
   #:static-vector
   #:maybe-free-static-vector
   #:static-vector-pointer)
  ;; subject.lisp
  (:export
   #:subject-class-redefined
   #:subject-class
   #:subject-class
   #:effective-handlers
   #:instances
   #:cascade-option-changes
   #:subject
   #:event-loop
   #:regenerate-handlers
   #:define-subject
   #:define-handler
   #:define-generic-handler)
  ;; subjects.lisp
  (:export
   #:clocked-subject
   #:vertex-subject
   #:vertex-array
   #:vertex-form
   #:colored-subject
   #:color
   #:vertex-colored-subject
   #:textured-subject
   #:texture)
  ;; toolkit.lisp
  (:export
   #:finalize
   #:current-time
   #:executable-directory
   #:enlist
   #:unlist
   #:remf*
   #:with-retry-restart
   #:with-new-value-restart
   #:with-cleanup-on-failure
   #:*standalone*
   #:standalone-error-handler
   #:standalone-logging-handler
   #:make-thread
   #:with-thread
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-slots-bound
   #:with-all-slots-bound
   #:minimize
   #:symbol->c-name
   #:check-gl-type
   #:cl-type->gl-type
   #:gl-type->cl-type
   #:gl-coerce
   #:check-texture-size
   #:define-enum-check
   #:check-texture-target
   #:check-texture-mag-filter
   #:check-texture-min-filter
   #:check-texture-wrapping
   #:check-shader-type
   #:check-vertex-buffer-type
   #:check-vertex-buffer-element-type
   #:check-vertex-buffer-data-usage
   #:check-framebuffer-attachment)
  ;; transforms.lisp
  (:export
   #:view-matrix
   #:projection-matrix
   #:look-at
   #:perspective-projection
   #:orthographic-projection
   #:model-matrix
   #:push-matrix
   #:pop-matrix
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
   #:list-windows
   #:remove-window
   #:window
   #:name))

(defpackage #:cl+trial
  (:nicknames #:org.shirakumo.fraf.trial.cl+trial)
  (:shadowing-import-from #:trial #:scene #:entity #:load #:update)
  (:use #:cl #:trial #:3d-vectors #:flare))

(do-symbols (symb '#:cl+trial)
  (export symb '#:cl+trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+trial))
