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
   #:ui)
  (:export
   #:vec
   #:vec2
   #:vec3
   #:vec4
   #:define-set-representation
   #:asset
   #:resource
   #:video-mode))
