#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl #:3d-vectors #:3d-matrices #:flare)
  (:shadow #:scene #:entity #:load)
  ;; context.lisp
  (:export
   #:*context*
   #:with-context
   #:launch-with-context
   #:make-context
   #:context
   #:current-thread
   #:context-lock
   #:assets
   #:handler
   #:create-context
   #:valid-p
   #:destroy-context
   #:make-current
   #:done-current
   #:hide
   #:show
   #:acquire-context
   #:release-context
   #:resize
   #:swap-buffers
   #:hide-cursor
   #:title
   #:profile
   #:version
   #:resize
   #:width
   #:height)
  ;; display.lisp
  (:export
   #:display
   #:context
   #:clear-color
   #:setup-rendering
   #:render)
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
   #:issue
   #:process
   #:discard-events
   #:handler
   #:event-type
   #:container
   #:delivery-function
   #:priority
   #:event
   #:tick)
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
  ;; toolkit.lisp
  (:export
   #:*time-units*
   #:finalize
   #:current-time
   #:executable-directory
   #:enlist
   #:unlist
   #:with-retry-restart
   #:with-new-value-restart
   #:with-cleanup-on-failure
   #:*standalone*
   #:standalone-error-handler
   #:make-thread
   #:with-thread
   #:wait-for-thread-exit
   #:with-thread-exit
   #:with-error-logging
   #:with-timing-report
   #:check-gl-type
   #:gl-coerce
   #:check-texture-size
   #:check-texture-target
   #:check-texture-mag-filter
   #:check-texture-min-filter
   #:check-texture-wrapping
   #:check-shader-type
   #:check-vertex-buffer-type
   #:check-vertex-buffer-element-type
   #:check-vertex-buffer-data-usage
   #:check-framebuffer-attachment))

(flet ((reexport (in from)
         (do-external-symbols (symb in)
           (export (find-symbol (symbol-name symb) from) from))))
  (reexport '#:3d-vectors '#:trial)
  (reexport '#:flare '#:trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl #:trial)
  (:shadowing-import-from #:trial #:load))
