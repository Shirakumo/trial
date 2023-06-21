#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct particle
  (location :vec3)
  (velocity :vec3)
  (rotational-velocity :float)
  (life :float)
  (max-life :float)
  (size-begin :float)
  (size-end :float))

(define-gl-struct particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (particles (:array (:struct particle) size)))

(define-gl-struct dead-particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (dead-particles (:array :int size)))

(define-gl-struct alive-particle-buffer
  (size NIL :initarg :size :initform 1000 :reader size)
  (alive-particles (:array :int size)))

(define-gl-struct particle-counter-buffer
  (alive-count :int)
  (dead-count :int)
  (real-emit-count :int)
  (count :int))

(define-gl-struct particle-argument-buffer
  (emit-args (:array :int 3))
  (simulate-args (:array :int 3))
  (draw-args (:array :int 4))
  (sort-args (:array :int 3)))

(define-gl-struct particle-emitter-buffer
  (world-matrix :mat4)
  (emit-count :int)
  (mesh-index-count :int)
  (mesh-vertex-position-stride :int)
  (mesh-vertx-normal-stride :int)
  (randomness :float)
  (particle-size :float)
  (particle-scaling :float)
  (particle-rotation :float)
  (particle-random-factor :float)
  (particle-normal-factor :float)
  (particle-lifespan :float)
  (particle-lifespan-randomness :float))

(define-shader-pass particle-kickoff-pass (compute-pass)
  ())

(define-shader-pass particle-emit-pass (compute-pass)
  ())

(define-shader-pass particle-simulate-pass (compute-pass)
  ())

(define-shader-entity particle-emitter (standalone-shader-entity listener)
  (kickoff-pass
   emit-pass
   simulate-pass
   texture
   (max-particles :initarg :max-particles :initform 1000 :reader max-particles))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "particle/render.glsl")))

(defmethod initialize-instance :after ((emitter particle-emitter) &key)
  (with-all-slots-bound (emitter particle-emitter)
    (setf particle-emitter-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-emitter-buffer)))
    (setf particle-argument-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-argument-buffer)))
    (setf particle-counter-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-counter-buffer)))
    (setf alive-particle-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'alive-particle-buffer :size max-particles)))
    (setf alive-particle-buffer-back (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'alive-particle-buffer :size max-particles)))
    (setf dead-particle-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'dead-particle-buffer :size max-particles)))
    (setf particle-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-buffer :size max-particles)))
    
    (setf kickoff-pass (make-instance 'particle-kickoff-pass))
    (setf emit-pass (make-instance 'particle-emit-pass))
    (setf simulate-pass (make-instance 'particle-simulate-pass))))

(defmethod buffers ((emitter particle-emitter))
  (list* (slot-value emitter 'particle-buffer)
         (call-next-method)))

(defmethod stage :after ((emitter particle-emitter) (area staging-area))
  (stage (kickoff-pass emitter) area)
  (stage (emit-pass emitter) area)
  (stage (simulate-pass emitter) area)
  (stage (// 'trial 'empty-vertex-array) area))

(define-handler (particle-emitter tick) ()
  ;; Simulate with compute shaders
  (render (slot-value particle-emitter 'kickoff-pass) T)
  (render (slot-value particle-emitter 'emit-pass) T)
  (render (slot-value particle-emitter 'simulate-pass) T))

(defmethod render ((emitter particle-emitter) (program shader-program))
  (gl:bind-vertex-array (// 'trial 'empty-vertex-array))
  (let ((particle-count (slot-value (gl-struct (slot-value emitter 'particle-counter-buffer)) 'count)))
    (%gl:draw-arrays :triangles 0 (* particle-count 6)))
  (gl:bind-vertex-array 0))
