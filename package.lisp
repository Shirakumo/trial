#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl+qt #:3d-vectors #:3d-matrices #:flare)
  (:shadowing-import-from #:flare #:slot)
  (:shadow #:scene #:entity #:load)
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
   #:pool-path
   #:define-asset)
  ;; asset.lisp
  (:export
   #:load
   #:offload
   #:asset
   #:inputs
   #:resource
   #:finalize-resource
   #:coerce-input
   #:coerced-inputs
   #:make-asset
   #:with-assets
   #:with-assets*
   #:shader-asset
   #:shader-type
   #:shader-program-asset
   #:uniform
   #:vertex-buffer-asset
   #:buffer-type
   #:element-type
   #:data-usage
   #:size
   #:vertex-array-asset
   #:size
   #:packed-vao-asset
   #:texture-asset
   #:target
   #:mag-filter
   #:min-filter
   #:anisotropy
   #:wrapping
   #:framebuffer-asset
   #:framebuffer-bundle-asset
   #:resize)
  ;; attributes.lisp
  (:export
   #:attribute-table
   #:enable
   #:disable
   #:push-attribs
   #:pop-attribs
   #:with-pushed-attribs)
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
   #:sidescroll-camera
   #:3d-camera
   #:fov
   #:target-camera
   #:pivot-camera
   #:following-camera
   #:fps-camera
   #:freeroam-camera
   #:editor-camera)
  ;; collada.lisp
  (:export
   #:collada->vertex-format
   #:load-collada)
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
   #:display
   #:maybe-reload-scene)
  ;; debugging.lisp
  (:export
   #:reload-with-features)
  ;; dialog-system.lisp
  (:export
   #:dialogue
   #:dialogue-find
   #:dialogue-next
   #:define-dialogue)
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
  ;; effects.lisp
  (:export
   #:effects
   #:negative-pass
   #:grayscale-pass
   #:box-blur-pass
   #:sobel-pass
   #:gaussian-blur-pass
   #:fxaa-pass
   #:high-pass-filter
   #:low-pass-filter)
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
   #:skip-event
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
   #:fullscreen-square
   #:cube)
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
   #:pipeline
   #:scene
   #:hud
   #:title
   #:setup-scene
   #:setup-pipeline
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
  ;; pipeline.lisp
  (:export
   #:pipeline
   #:passes
   #:connections
   #:framebuffers
   #:pass-fbo-map
   #:register
   #:deregister
   #:clear
   #:connect-pass
   #:pack-pipeline)
  ;; player.lisp
  (:export)
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
  ;; shader-pass.lisp
  (:export
   #:shader-pass-class
   #:pass-inputs
   #:shader-pass
   #:register-object-for-pass
   #:shader-program-for-pass
   #:define-shader-pass
   #:per-object-pass
   #:assets
   #:multisampled-pass
   #:multisampled-per-object-pass
   #:single-shader-pass
   #:post-effect-pass
   #:copy-pass)
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
   #:subject-class-redefined
   #:subject-class
   #:effective-handlers
   #:instances
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
   #:colored-subject
   #:vertex-colored-subject
   #:textured-subject)
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
   #:screen->vec
   #:vec->main
   #:main->vec)
  ;; vertex-format.lisp
  (:export
   #:write-vformat
   #:load-vformat
   #:vertex-format-asset)
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
  (:shadowing-import-from #:trial #:load)
  (:shadowing-import-from #:flare #:slot))
