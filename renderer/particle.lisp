#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct particle
  ())

(define-gl-struct particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (particles (:array (:struct particle) size)))

(define-gl-struct dead-particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (particles (:array :int size)))

(define-gl-struct alive-particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (particles (:array :int size)))

(define-gl-struct particle-counter-buffer
  (alive :int)
  (dead :int)
  (real-emit-count :int)
  (count :int))

(define-gl-struct particle-argument-buffer
  (emit-args (:array :int 3))
  (simulate-args (:array :int 3))
  (draw-args (:array :int 4))
  (sort-args (:array :int 3)))

(define-shader-entity particle-emitter (standalone-shader-entity)
  (kickoff-pass
   emit-pass
   simulate-pass
   texture
   max-particles)
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "particle/render.glsl")))

(defmethod stage :after ((emitter particle-emitter) (area staging-area))
  (stage (kickoff-pass emitter) area)
  (stage (emit-pass emitter) area)
  (stage (simulate-pass emitter) area)
  (stage (// 'trial 'empty-vertex-array) area))
