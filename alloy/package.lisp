#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.alloy
  (:use #:cl)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:animation #:org.shirakumo.alloy.animation))
  (:export
   #:renderer
   #:event-bridge
   #:ui
   #:ui-pass)
  (:export
   #:ui-actions
   #:select-left
   #:select-right
   #:select-up
   #:select-down
   #:accept
   #:back)
  (:export
   #:base-ui
   #:panels
   #:find-panel
   #:toggle-panel
   #:show-panel
   #:hide-panel
   #:panel
   #:shown-p
   #:fullscreen-panel
   #:action-set-change-panel
   #:menuing-panel)
  (:export
   #:language-data
   #:vec
   #:vec2
   #:vec3
   #:vec4
   #:define-set-representation
   #:asset
   #:resource
   #:monitor
   #:video-mode))
