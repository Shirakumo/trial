#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipelined-scene (scene pipeline)
  ())

(defmethod leave :after ((entity entity) (scene pipelined-scene))
  ;; FIXME: A system for figuring out when we can GC shader programs
  )

(defmethod setup-scene :after (main (scene scene))
  (pack-pipeline scene (context main)))

(defmethod stage :before ((scene pipelined-scene) (area staging-area))
  (compile-to-pass scene scene))

(defmethod stage :after ((scene pipelined-scene) (area staging-area))
  (loop for texture across (textures scene)
        do (stage texture area))
  (loop for pass across (passes scene)
        do (stage pass area)))

(defmethod handle :after ((event resize) (scene pipelined-scene))
  (resize scene (width event) (height event)))
