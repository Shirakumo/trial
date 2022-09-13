#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipelined-scene (scene pipeline)
  ())

(defmethod setup-scene :after (main (scene scene))
  (pack-pipeline scene (context main))
  (for:for ((object over scene))
    (loop for pass across (passes scene)
          do (enter object pass))))

(defmethod stage :after ((scene pipelined-scene) (area staging-area))
  (loop for texture across (textures scene)
        do (stage texture area))
  (loop for pass across (passes scene)
        do (stage pass area)))

(defmethod handle :after ((event resize) (scene pipelined-scene))
  (resize scene (width event) (height event)))

(defmethod enter :after ((entity renderable) (scene pipelined-scene))
  (loop for pass across (passes scene)
        do (enter entity pass)))

(defmethod leave :after ((entity renderable) (scene pipelined-scene))
  (loop for pass across (passes scene)
        do (leave entity pass)))

(defmethod enter :after ((entity container) (scene pipelined-scene))
  (loop for pass across (passes scene)
        do (enter entity pass)))

(defmethod leave :after ((entity container) (scene pipelined-scene))
  (loop for pass across (passes scene)
        do (leave entity pass)))
