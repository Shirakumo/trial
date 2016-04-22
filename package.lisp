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
   #:font
   #:size
   #:family
   #:gl-asset
   #:file-asset
   #:file
   #:image
   #:texture
   #:target
   #:filter
   #:wrapping
   #:model
   #:shader
   #:shader-type
   #:pathname->shader-type
   #:shader-program
   #:shaders
   #:vertex-buffer
   #:buffer-type
   #:element-type
   #:buffer-data
   #:data-usage
   #:vertex-array
   #:buffers)
  ;; asset-pool.lisp
  (:export)
  ;; assets.lisp
  (:export
   #:pool
   #:remove-pool
   #:pools
   #:pool
   #:name
   #:base
   #:assets
   #:enter
   #:leave
   #:asset
   #:define-pool
   #:asset
   #:name
   #:home
   #:resource
   #:resource-type
   #:data
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
   #:camera
   #:target
   #:up
   #:fov
   #:project-view
   #:look-at
   #:pivot-camera
   #:following-camera)
  ;; context.lisp
  (:export
   #:context
   #:context
   #:current-thread
   #:context-lock
   #:acquire-context
   #:release-context
   #:with-context)
  ;; controller.lisp
  (:export
   #:controller
   #:tick-count
   #:update-thread
   #:setup-rendering
   #:render
   #:render-hud
   #:setup-scene
   #:execute
   #:func
   #:bindings
   #:result
   #:execute
   #:funcall-in-scene)
  ;; debugging.lisp
  (:export
   #:*debug-features*
   #:*optimize-features*
   #:reload-with-features)
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
   #:queue
   #:issue
   #:process
   #:handler
   #:event-type
   #:container
   #:delivery-function
   #:priority)
  ;; flare.lisp
  (:export)
  ;; framebuffer.lisp
  (:export
   #:attachment-value
   #:framebuffer
   #:buffer-object
   #:buffer-format
   #:call-with-framebuffer-bound
   #:with-framebuffer-bound
   #:width
   #:height
   #:texture)
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
   #:min-bound
   #:max-bound
   #:contains
   #:intersects
   #:intersection-entity
   #:first-object
   #:second-object
   #:distance
   #:normal
   #:ray
   #:collidable-entity
   #:handle-collision
   #:colored-entity
   #:textured-entity
   #:mesh-entity
   #:mesh
   #:shader-entity
   #:shader-program
   #:face-entity
   #:tex-location
   #:tex-bounds)
  ;; input-tables.lisp
  (:export
   #:*key-table*
   #:*button-table*
   #:*gamepad-device-table*
   #:*gamepad-axis-table*
   #:*gamepad-button-table*
   #:define-gamepad
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
   #:mouse-move
   #:old-pos
   #:gamepad-event
   #:device
   #:gamepad-attach
   #:gamepad-remove
   #:gamepad-press
   #:gamepad-release
   #:gamepad-move)
  ;; mapping.lisp
  (:export
   #:mapping
   #:remove-mapping
   #:define-mapping
   #:define-simple-mapping
   #:map-event
   #:action
   #:remove-action-mappings
   #:define-action
   #:system-action
   #:launch-editor
   #:save-game
   #:load-game
   #:reload-assets
   #:reload-scene
   #:player-action
   #:movement
   #:start-left
   #:start-right
   #:start-up
   #:start-down
   #:stop-left
   #:stop-right
   #:stop-up
   #:stop-down
   #:perform)
  ;; player.lisp
  (:export)
  ;; scene.lisp
  (:export
   #:scene
   #:scene-event
   #:scene
   #:enter
   #:entity
   #:leave)
  ;; selectable.lisp
  (:export
   #:register-object-color
   #:color->object
   #:selection-buffer
   #:object-at-point
   #:selectable-entity
   #:color-id)
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
  ;; storage.lisp
  (:export
   #:*pack-compile*
   #:ins
   #:savable
   #:save-form-args
   #:save-form
   #:pack
   #:unpack)
  ;; subject.lisp
  (:export
   #:define-handler
   #:define-generic-handler
   #:subject-class
   #:effective-handlers
   #:instances
   #:subject
   #:loops
   #:define-subject)
  ;; subjects.lisp
  (:export
   #:clocked-subject)
  ;; toolkit.lisp
  (:export
   #:*time-units*
   #:current-time
   #:enlist
   #:unlist
   #:with-primitives
   #:with-pushed-matrix
   #:matirx-4x4
   #:one-of
   #:make-painter
   #:with-painter
   #:input-source
   #:input-value
   #:input-literal
   #:with-retry-restart
   #:with-new-value-restart
   #:with-cleanup-on-failure
   #:acquire-lock-with-starvation-test
   #:check-texture-size
   #:check-texture-target
   #:check-shader-type
   #:check-vertex-buffer-type
   #:check-vertex-buffer-element-type
   #:check-vertex-buffer-data-usage)
  ;; windowing.lisp
  (:export
   #:main
   #:scene
   #:controller
   #:resize
   #:width
   #:height
   #:funcall-in-gui
   #:launch))

(flet ((reexport (in from)
         (do-external-symbols (symb in)
           (export (find-symbol (symbol-name symb) from) from))))
  (reexport '#:3d-vectors '#:trial)
  (reexport '#:flare '#:trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+qt #:trial)
  (:shadowing-import-from #:flare #:slot))
