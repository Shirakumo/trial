#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (particle-force-field (:layout-standard std430))
  (type :int :initform 0)
  (position :vec3 :initform (vec 0 0 0))
  (strength :float :initform 0.0)
  (inv-range :float :initform 0.0)
  (normal :vec3 :initform (vec 0 0 0)))

(define-gl-struct (particle-force-fields (:layout-standard std430))
  (size NIL :initarg :size :initform 32 :reader size)
  (particle-force-field-count :int :initform 0 :accessor particle-force-field-count)
  (particle-force-fields (:array (:struct particle-force-field) size) :reader particle-force-fields))

(define-gl-struct (particle-struct (:layout-standard std430))
  (position :vec3)
  (velocity :vec3)
  (rotational-velocity :float)
  (life :float)
  (max-life :float)
  (size-begin :float)
  (size-end :float))

(define-gl-struct (particle-buffer (:layout-standard std430))
  (size NIL :initarg :size :initform 1000 :reader size)
  (particles (:array (:struct particle-struct) size)))

(define-gl-struct (particle-index-buffer (:layout-standard std430))
  (size NIL :initarg :size :initform 1000 :reader size)
  (indices (:array :int size)))

(define-gl-struct (particle-counter-buffer (:layout-standard std430))
  (alive-count :int)
  (dead-count :int)
  (real-emit-count :int)
  (total-count :int))

(define-gl-struct (particle-argument-buffer (:layout-standard std430))
  (emit-args :uvec3)
  (simulate-args :uvec3)
  (draw-args :uvec4)
  (sort-args :uvec3))

(define-gl-struct (particle-emitter-buffer (:layout-standard std430))
  (model-matrix :mat4 :accessor transform-matrix)
  (emit-count :int :initform 0 :accessor emit-count)
  (mesh-index-count :int :initform 0 :accessor mesh-index-count)
  (mesh-vertex-position-stride :int :initform 0 :accessor mesh-vertex-position-stride)
  (mesh-vertex-normal-stride :int :initform 0 :accessor mesh-vertex-normal-stride)
  (randomness :float :initform 0.0 :accessor randomness)
  (particle-size :float :initform 1.0 :accessor particle-size)
  (particle-scaling :float :initform 1.0 :accessor particle-scaling)
  (particle-rotation :float :initform 1.0 :accessor particle-rotation)
  (particle-random-factor :float :initform 0.0 :accessor particle-random-factor)
  (particle-normal-factor :float :initform 1.0 :accessor particle-normal-factor)
  (particle-lifespan :float :initform 1.0 :accessor particle-lifespan)
  (particle-lifespan-randomness :float :initform 0.0 :accessor particle-lifespan-randomness))

(define-shader-pass particle-kickoff-pass (compute-pass)
  ((emit-threads :constant T :initform 256 :initarg :emit-threads)
   (simulate-threads :constant T :initform 256 :initarg :simulate-threads)
   (particle-counter-buffer :buffer T :initarg :particle-counter-buffer)
   (particle-argument-buffer :buffer T :initarg :particle-argument-buffer))
  (:shader-file (trial "particle/kickoff.glsl")))

(define-shader-pass particle-emit-pass (compute-pass)
  ((emit-threads :constant T :initform 256 :initarg :emit-threads)
   (particle-buffer :buffer T :initarg :particle-buffer)
   (alive-particle-buffer-0 :buffer T :initarg :alive-particle-buffer-0)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1)
   (dead-particle-buffer :buffer T :initarg :dead-particle-buffer)
   (particle-counter-buffer :buffer T :initarg :particle-counter-buffer))
  (:shader-file (trial "particle/emit.glsl")))

(define-shader-pass particle-simulate-pass (compute-pass)
  ((simulate-threads :constant T :initform 256 :initarg :simulate-threads)
   (particle-buffer :buffer T :initarg :particle-buffer)
   (particle-force-fields :buffer T :initarg :particle-force-fields)
   (alive-particle-buffer-0 :buffer T :initarg :alive-particle-buffer-0)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1)
   (dead-particle-buffer :buffer T :initarg :dead-particle-buffer)
   (particle-counter-buffer :buffer T :initarg :particle-counter-buffer)
   (particle-argument-buffer :buffer T :initarg :particle-argument-buffer))
  (:shader-file (trial "particle/simulate.glsl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shader-entity particle-emitter (standalone-shader-entity listener)
    ((particle-emitter-buffer)
     (particle-force-fields)
     (particle-argument-buffer)
     (particle-counter-buffer)
     (alive-particle-buffer-0)
     (alive-particle-buffer-1)
     (dead-particle-buffer)
     (particle-buffer :buffer :read)
     (kickoff-pass)
     (emit-pass)
     (simulate-pass)
     (texture)
     (vertex-array :initarg :vertex-array :initform (// 'trial 'unit-square) :accessor vertex-array)
     (max-particles :initarg :max-particles :initform 1000 :reader max-particles))
    (:buffers (trial standard-environment-information))
    (:shader-file (trial "particle/render.glsl"))))

(defmethod initialize-instance :after ((emitter particle-emitter) &key (emit-threads 256) (simulate-threads 256) particle-force-fields)
  (setf (slot-value emitter 'particle-force-fields)
        (or particle-force-fields
            (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-force-fields))))
  (with-all-slots-bound (emitter particle-emitter)
    (setf particle-emitter-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-emitter-buffer)))
    (setf particle-argument-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-argument-buffer)))
    (setf particle-counter-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-counter-buffer)))
    (setf alive-particle-buffer-0 (make-instance 'shader-storage-buffer :binding "alive_particles" :struct (make-instance 'particle-index-buffer :size max-particles)))
    (setf alive-particle-buffer-1 (make-instance 'shader-storage-buffer :binding "alive_particles" :struct (make-instance 'particle-index-buffer :size max-particles)))
    (setf dead-particle-buffer (make-instance 'shader-storage-buffer :binding "dead_particles" :struct (make-instance 'particle-index-buffer :size max-particles)))
    (setf particle-buffer (make-instance 'shader-storage-buffer :binding NIL :struct (make-instance 'particle-buffer :size max-particles)))
    
    (setf kickoff-pass (make-instance 'particle-kickoff-pass :emit-threads emit-threads :simulate-threads simulate-threads :particle-counter-buffer particle-counter-buffer :particle-argument-buffer particle-argument-buffer))
    (setf emit-pass (make-instance 'particle-emit-pass :emit-threads emit-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer
                                                       :work-groups 0))
    (setf simulate-pass (make-instance 'particle-simulate-pass :simulate-threads simulate-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer
                                                               :work-groups (* 3 4)))))

(defmethod stage :after ((emitter particle-emitter) (area staging-area))
  (stage (kickoff-pass emitter) area)
  (stage (emit-pass emitter) area)
  (stage (simulate-pass emitter) area)
  (stage (// 'trial 'empty-vertex-array) area))

(defmethod (setf vertex-array) :after (vao (emitter particle-emitter))
  (with-buffer-tx (struct (slot-value emitter 'particle-emitter-buffer) :update NIL)
    (setf (mesh-index-count struct) (size vao))
    (setf (mesh-vertex-position-stride struct)
          (loop for binding in (bindings struct)
                do (when (and (listp binding) (= 0 (getf (rest binding) :index)))
                     (return (getf binding :stride)))))
    (setf (mesh-vertex-normal-stride struct)
          (loop for binding in (bindings struct)
                do (when (and (listp binding) (= 1 (getf (rest binding) :index)))
                     (return (getf binding :stride)))))))

(macrolet ((define-delegate (accessor)
             `(progn (defmethod ,accessor ((emitter particle-emitter))
                       (,accessor (struct (slot-value emitter 'particle-emitter-buffer))))

                     (defmethod (setf ,accessor) (value (emitter particle-emitter))
                       (setf (,accessor (struct (slot-value emitter 'particle-emitter-buffer))) value)))))
  (define-delegate particle-size)
  (define-delegate particle-scaling)
  (define-delegate particle-rotation)
  (define-delegate particle-random-factor)
  (define-delegate particle-normal-factor)
  (define-delegate particle-lifespan)
  (define-delegate particle-lifespan-randomness))

(define-handler (particle-emitter tick) ()
  (with-all-slots-bound (particle-emitter particle-emitter)
    (with-buffer-tx (struct particle-emitter-buffer)
      (setf (transform-matrix struct) (model-matrix))
      (setf (emit-count struct) 0)
      (setf (randomness struct) (random 1.0)))
    ;; Simulate with compute shaders
    (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
    (render kickoff-pass T)
    ;; Bind the VAO somehow?
    (render emit-pass T)
    (render simulate-pass T)
    ;; Swap the buffers
    (rotatef (gl-name alive-particle-buffer-0)
             (gl-name alive-particle-buffer-1))))

(defmethod render ((emitter particle-emitter) (program shader-program))
  (gl:bind-vertex-array (// 'trial 'empty-vertex-array))
  (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
  (%gl:draw-arrays-indirect :triangles (* 2 3 4))
  (gl:bind-vertex-array 0))

;; https://github.com/turanszkij/WickedEngine/tree/9caf25a52996c6c62fc39f10784d8951f715b05d/WickedEngine
