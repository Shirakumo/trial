#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipelined-scene (scene pipeline bakable)
  ())

(defmethod leave :after ((entity entity) (scene pipelined-scene))
  ;; FIXME: A system for figuring out when we can GC shader programs
  )

(defmethod bake ((scene pipelined-scene))
  (pack-pipeline scene *context*)
  (compile-to-pass scene scene))

(defmethod compute-resources ((scene pipelined-scene) resources readying cache)
  (compute-resources (objects scene) resources readying cache)
  (compute-resources (textures scene) resources readying cache)
  (compute-resources (passes scene) resources readying cache))

(defmethod handle :after ((event resize) (scene pipelined-scene))
  (resize scene (width event) (height event)))
