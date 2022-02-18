#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.animation
  (:use #:cl
        #:org.shirakumo.flare.vector
        #:org.shirakumo.flare.matrix
        #:org.shirakumo.flare.quaternion
        #:org.shirakumo.flare.transform)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:gltf #:org.shirakumo.fraf.gltf)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)))
