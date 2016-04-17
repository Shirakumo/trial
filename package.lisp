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
  ;; assets.lisp
  (:export
   #:*assets*
   #:*root*
   #:resource-pathname
   #:clear-assets
   #:asset
   #:state
   #:data
   #:asset
   #:remove-asset
   #:content
   #:restore
   #:offload
   #:file-asset
   #:file
   #:allowed-types
   #:image
   #:texture
   #:target
   #:filter
   #:wrapping
   #:sound
   #:model
   #:shader
   #:shader-type
   #:shader-program
   #:shaders
   #:gl-buffer
   #:buffer-type
   #:element-type
   #:buffer-data
   #:data-usage)
  ;; camera.lisp
  (:export
   #:camera
   #:target
   #:up
   #:look-at
   #:pivot-camera
   #:following-camera)
  ;; controller.lisp
  (:export
   #:controller
   #:tickcount
   #:update-thread
   #:last-pause
   #:fps
   #:perspective-view
   #:setup-scene
   #:execute
   #:func
   #:bindings
   #:result
   #:execute
   #:funcall-in-scene)
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
   #:handler
   #:event-type
   #:container
   #:delivery-function
   #:priority
   #:define-handler
   #:subject-class
   #:subject
   #:loops
   #:define-subject
   #:event
   #:tick)
  ;; flare.lisp
  (:export)
  ;; input-tables.lisp
  (:export
   #:*key-table*
   #:*mouse-button-table*
   #:*gamepad-device-table*
   #:*gamepad-axis-table*
   #:*gamepad-button-table*
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
   #:mouse-button-event
   #:button
   #:mouse-press
   #:mouse-release
   #:mouse-move
   #:old-pos
   #:pos
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
   #:pos)
  ;; mapping.lisp
  (:export
   #:mapping
   #:remove-mapping
   #:define-mapping
   #:define-simple-mapping
   #:map-event
   #:action
   #:remove-aciton-mappings
   #:define-action
   #:system-action
   #:launch-editor
   #:player-action
   #:movement
   #:start-left
   #:start-right
   #:start-up
   #:start-down
   #:stop-left
   #:stop-right
   #:stop-up
   #:stop-down)
  ;; player.lisp
  (:export
   #:player)
  ;; scene.lisp
  (:export
   #:scene
   #:scene-event
   #:scene
   #:enter
   #:subject
   #:leave)
  ;; sprite.lisp
  (:export
   #:sprite-animation
   #:duration
   #:frames
   #:frame
   #:next
   #:width
   #:height
   #:sprite-subject
   #:animations
   #:animation)
  ;; storage.lisp
  (:export
   #:*unpack-target*
   #:*pack-compile*
   #:ins
   #:savable
   #:save
   #:pack
   #:unpack)
  ;; subjects.lisp
  (:export
   #:textured-subject
   #:texture
   #:bind-texture
   #:located-subject
   #:location
   #:pivoted-subject
   #:pivot
   #:oriented-subject
   #:orientation
   #:bound-subject
   #:bounds
   #:contains
   #:intersects
   #:intersection-subject
   #:collidable-subject
   #:handle-collision
   #:up
   #:rotated-subject
   #:axis
   #:angle
   #:mesh-subject
   #:mesh
   #:space-axes
   #:size
   #:grid
   #:clocked-subject)
  ;; toolkit.lisp
  (:export
   #:*time-units*
   #:current-time
   #:enlist
   #:unlist
   #:with-primitives
   #:with-pushed-matrix
   #:matrix-4x4
   #:one-of)
  ;; windowing.lisp
  (:export
   #:*main*
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
