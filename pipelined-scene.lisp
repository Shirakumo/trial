#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipelined-scene (scene pipeline bakable)
  ())

(defmethod paint ((scene pipelined-scene) target)
  (paint-with scene scene))

(defmethod paint ((scene pipelined-scene) (target shader-pass))
  (for:for ((element over scene))
    (paint-with target element)))

(defmethod enter :after ((entity entity) (scene pipelined-scene))
  (register-object-for-pass scene entity))

(defmethod leave :after ((entity entity) (scene pipelined-scene))
  ;; FIXME: A system for figuring out when we can GC shader programs
  ;; (deregister-object-for-pass scene entity)
  )

(defmethod bake ((scene pipelined-scene))
  (pack-pipeline scene *context*)
  (for:for ((element over scene))
    (register-object-for-pass scene element)))

(defmethod handle :after ((event resize) (scene pipelined-scene))
  (resize scene (width event) (height event)))
