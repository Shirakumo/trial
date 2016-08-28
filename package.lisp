#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl+qt #:3d-vectors #:flare)
  (:shadowing-import-from #:flare #:slot)
  (:shadow #:scene #:entity)
  ;; asset-classes.lisp
  (:export
   #:context-asset
   #:resource
   #:file-asset
   #:file
   #:image
   #:font
   #:size
   #:family
   #:texture
   #:target
   #:mag-filter
   #:min-filter
   #:anisotropy
   #:wrapping
   #:model
   #:texture-map
   #:texture-store
   #:shader
   #:shader-type
   #:shader-program
   #:shaders
   #:vertex-buffer
   #:buffer-type
   #:element-type
   #:buffer-data
   #:data-usage
   #:vertex-array
   #:buffers
   #:framebuffer
   #:attachment
   #:width
   #:height
   #:mipmap
   #:samples)
  ;; asset-pool.lisp
  (:export)
  ;; assets.lisp
  (:export
   #:*standalone*
   #:pool
   #:remove-pool
   #:pools
   #:resolve-pool-base
   #:reconfigure-pool-bases
   #:pool
   #:name
   #:base-designator
   #:base
   #:assets
   #:enter
   #:leave
   #:asset
   #:restore
   #:offload
   #:define-pool
   #:asset
   #:name
   #:home
   #:resource
   #:dependencies
   #:loaded-p
   #:data
   #:matches
   #:reload
   #:restore
   #:offload
   #:load-data
   #:finalize-data
   #:get-resource
   #:define-asset
   #:resource
   #:data
   #:resource-asset)
  ;; camera.lisp
  (:export
   #:perspective-view
   #:look-at
   #:camera
   #:near-plane
   #:far-plane
   #:project-view
   #:setup-perspective
   #:2d-camera
   #:3d-camera
   #:fov
   #:target-camera
   #:pivot-camera
   #:following-camera
   #:fps-camera
   #:freeroam-camera
   #:editor-camera)
  ;; context.lisp
  (:export
   #:*context*
   #:with-context
   #:context
   #:glformat
   #:glcontext
   #:current-thread
   #:context-waiting
   #:context-lock
   #:context-wait-lock
   #:context-needs-recreation
   #:construct
   #:accumulation-buffer
   #:alpha-buffer
   #:depth-buffer
   #:stencil-buffer
   #:stereo-buffer
   #:direct-rendering
   #:double-buffering
   #:overlay
   #:plane
   #:multisampling
   #:samples
   #:swap-interval
   #:profile
   #:version
   #:destroy-context
   #:create-context
   #:acquire-context
   #:release-context)
  ;; controller.lisp
  (:export
   #:system-action
   #:launch-editor
   #:save-game
   #:load-game
   #:reload-assets
   #:reload-scene
   #:controller
   #:tick-count
   #:display)
  ;; debugging.lisp
  (:export
   #:reload-with-features)
  ;; display.lisp
  (:export
   #:display
   #:clear-color
   #:handle
   #:resize
   #:width
   #:height
   #:setup-rendering
   #:paint
   #:render)
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
   #:event-loop
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
   #:pause
   #:resume)
  ;; executable.lisp
  (:export
   #:execute-request
   #:func
   #:bindings
   #:result
   #:execute
   #:with-execution
   #:executable
   #:funcall-in-gui
   #:with-body-in-gui)
  ;; flare.lisp
  (:export)
  ;; fullscreenable.lisp
  (:export
   #:fullscreenable
   #:original-mode
   #:resolution
   #:fullscreen)
  ;; geometry.lisp
  (:export
   #:geometry
   #:bounded-geometry
   #:width
   #:height
   #:sized-geometry
   #:size
   #:segmented-geometry
   #:segments
   #:triangle
   #:square
   #:rectangle
   #:disc
   #:sphere
   #:cube
   #:cylinder
   #:space-axes)
  ;; helpers.lisp
  (:export
   #:located-entity
   #:location
   #:oriented-entity
   #:orientation
   #:up
   #:rotated-entity
   #:axis
   #:angle
   #:pivoted-entity
   #:pivot
   #:bound-entity
   #:bounds
   #:colored-entity
   #:color
   #:textured-entity
   #:texture
   #:mesh-entity
   #:mesh
   #:shader-entity
   #:shader-program
   #:face-entity
   #:tex-location
   #:tex-bounds)
  ;; hud.lisp
  (:export
   #:hud
   #:hud-entity
   #:hud-layer)
  ;; input-tables.lisp
  (:export
   #:*key-table*
   #:*button-table*
   #:*gamepad-device-table*
   #:*gamepad-axis-table*
   #:*gamepad-button-table*
   #:define-gamepad
   #:xbox-360
   #:logitech-f310
   #:dualshock-3
   #:buffalo-bsgp801
   #:steam-controller
   #:qt-key->symbol
   #:qt-button->symbol
   #:gamepad-axis->symbol
   #:gamepad-button->symbol)
  ;; input.lisp
  (:export
   #:input-event
   #:keyboard-event
   #:key
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
   #:device
   #:gamepad-attach
   #:gamepad-remove
   #:gamepad-press
   #:gamepad-release
   #:gamepad-move
   #:axis
   #:input-handler
   #:init-input-system
   #:shutdown-input-system
   #:key
   #:mouse
   #:gamepad)
  ;; launcher.lisp
  (:export
   #:launcher
   #:init-options)
  ;; layer-set.lisp
  (:export
   #:layer-container
   #:layer
   #:active
   #:layer-set
   #:enter
   #:leave
   #:paint
   #:layer-active-p
   #:layered-unit
   #:layer)
  ;; main.lisp
  (:export
   #:main
   #:scene
   #:hud
   #:setup-scene
   #:launch
   #:launch-with-launcher)
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
  ;; octree.lisp
  (:export)
  ;; octree2.lisp
  (:export)
  ;; player.lisp
  (:export)
  ;; projection.lisp
  (:export
   #:modelview-matrix
   #:projection-matrix
   #:proj-matrix
   #:view-matrix
   #:vec->screen
   #:screen->vec
   #:vec->main
   #:main->vec)
  ;; renderable.lisp
  (:export
   #:renderable
   #:thread
   #:last-pause
   #:last-duration
   #:target-fps
   #:actual-fps
   #:call-with-frame-pause
   #:with-frame-pause
   #:render-loop)
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
  ;; savestate.lisp
  (:export
   #:*compile-savestate*
   #:serialize
   #:restoration-slots
   #:restoration-initargs
   #:make-restore-form
   #:@=>
   #:@
   #:define-saved-slots
   #:define-saved-initargs
   #:unsavable
   #:persistent
   #:save-scene
   #:load-scene)
  ;; scene.lisp
  (:export
   #:scene
   #:scene-event
   #:scene
   #:enter
   #:leave
   #:entity
   #:register
   #:deregister
   #:funcall-in-scene
   #:with-body-in-scene)
  ;; selectable.lisp
  (:export
   #:ensure-color
   #:selection-buffer
   #:selected
   #:name-map
   #:color-map
   #:next-id
   #:register-object-color
   #:color->object
   #:object->color
   #:object-at-point
   #:object-at-mouse
   #:mouse-press-entity
   #:entity
   #:button
   #:mouse-release-entity
   #:global-selection-buffer
   #:color-id-entity
   #:color-id
   #:selectable-entity
   #:selected
   #:draggable-entity
   #:held
   #:drag)
  ;; skybox.lisp
  (:export
   #:skybox)
  ;; sprite.lisp
  (:export
   #:sprite-animation
   #:duration
   #:frames
   #:frame
   #:next
   #:sprite-subject
   #:animations
   #:animation)
  ;; subject.lisp
  (:export
   #:subject-class
   #:effective-handlers
   #:instances
   #:subject
   #:loops
   #:regenerate-handlers
   #:define-subject
   #:define-handler
   #:define-generic-handler)
  ;; subjects.lisp
  (:export
   #:clocked-subject)
  ;; toolkit.lisp
  (:export
   #:*time-units*
   #:current-time
   #:executable-directory
   #:enlist
   #:unlist
   #:with-primitives
   #:with-pushed-matrix
   #:with-pushed-attribs
   #:mkarray
   #:mktable
   #:mkobject
   #:update-slots
   #:matrix-4x4
   #:v4
   #:width
   #:height
   #:one-of
   #:with-painter
   #:input-source
   #:input-value
   #:input-literal
   #:with-retry-restart
   #:with-new-value-restart
   #:with-cleanup-on-failure
   #:acquire-lock-with-starvation-test
   #:make-thread
   #:with-thread
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-timing-report
   #:insert-index
   #:check-texture-size
   #:check-texture-target
   #:check-texture-mag-filter
   #:check-texture-min-filter
   #:check-shader-type
   #:check-vertex-buffer-type
   #:check-vertex-buffer-element-type
   #:check-vertex-buffer-data-usage
   #:check-framebuffer-attachment)
  ;; window.lisp
  (:export
   #:window-name
   #:window
   #:list-windows
   #:remove-window
   #:window
   #:name))

(flet ((reexport (in from)
         (do-external-symbols (symb in)
           (export (find-symbol (symbol-name symb) from) from))))
  (reexport '#:3d-vectors '#:trial)
  (reexport '#:flare '#:trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+qt #:trial)
  (:shadowing-import-from #:flare #:slot))
