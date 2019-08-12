#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.alloy
  (:use #:cl #:3d-vectors #:3d-matrices)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl))
  (:export
   #:renderer
   #:ui))
