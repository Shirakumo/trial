#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;;;; IMPLEMENTATION NOTES:
;;; This particle system is based on the one presented in [1], as
;;; part of the "Wicked Engine", specifically at this commit [2].
;;; Our implementation does not differ greatly from theirs, except
;;; to accommodate various trial-isms. According to my very basic
;;; tests, we can run around 100'000 particles with this on an old
;;; Nvidia 1050 Ti with nothing else going on. Going higher starts
;;; to dip into the frames, especially while recording.  Not sure
;;; if it's worth optimising this even further, though.
;;;
;;; The basic idea is to perform simulation in four steps:
;;; 1. Configure the internal dispatch buffers in a "kickoff pass"
;;; 2. Emit new particles and configure the current alive particle list
;;;    This will consume particles from the dead particle list
;;; 3. Simulate alive particles and put them onto the next alive list
;;;    Dead particles will be put onto the dead particle list
;;; 4. Swap the new and old alive lists
;;;
;;; Rendering then just goes over the alive particle list, making
;;; a quad for each.
;;;
;;; [1] https://wickedengine.net/2017/11/07/gpu-based-particle-simulation/
;;; [2] https://github.com/turanszkij/WickedEngine/tree/9caf25a52996c6c62fc39f10784d8951f715b05d/WickedEngine

(define-gl-struct (particle-force-field :layout-standard std430)
  (type :int :initform 0)
  (position :vec3 :initform (vec 0 0 0))
  (strength :float :initform 0.0)
  (range :float :initform 0.0)
  (inv-range :float :initform 0.0)
  (normal :vec3 :initform (vec 0 0 0)))

(define-gl-struct (particle-force-fields :layout-standard std430)
  (size NIL :initarg :size :initform 32 :reader size)
  (particle-force-field-count :int :initform 0 :accessor particle-force-field-count)
  (particle-force-fields (:array (:struct particle-force-field) size) :reader particle-force-fields))

(define-gl-struct (particle-struct :layout-standard std430 :gl-type "Particle")
  (position :vec3)
  (velocity :vec3)
  (rotational-velocity :float)
  (life :float)
  (max-life :float)
  (size-begin :float)
  (size-end :float)
  (color :uint))

(define-gl-struct (particle-buffer :layout-standard std430)
  (size NIL :initarg :size :reader size)
  (particles (:array (:struct particle-struct) size)))

(define-gl-struct (particle-counter-buffer :layout-standard std430)
  (alive-count :uint :initform 0)
  (dead-count :uint :initarg :max-particles)
  (real-emit-count :uint :initform 0)
  (total-count :uint :initform 0))

(define-gl-struct (particle-argument-buffer :layout-standard std140)
  (emit-args :uvec3 :initform (vec 0 0 0))
  (simulate-args :uvec3 :initform (vec 0 0 0))
  (draw-args :uvec4 :initform (vec 0 0 0 0))
  (sort-args :uvec3 :initform (vec 0 0 0)))

(define-gl-struct (particle-emitter-buffer :layout-standard std430)
  (model-matrix :mat4 :accessor transform-matrix)
  (emit-count :int :initform 0 :accessor emit-count)
  (mesh-index-count :int :initform 0 :accessor mesh-index-count)
  (mesh-vertex-position-stride :int :initform 0 :accessor mesh-vertex-position-stride)
  (mesh-vertex-normal-stride :int :initform 0 :accessor mesh-vertex-normal-stride)
  (randomness :float :initform 0.0 :accessor randomness)
  (particle-color :uint :initform #xFFFFFF :accessor particle-color)
  (particle-size :float :initform 1.0 :accessor particle-size)
  (particle-scaling :float :initform 1.0 :accessor particle-scaling)
  (particle-rotation :float :initform 1.0 :accessor particle-rotation)
  (particle-randomness :float :initform 0.0 :accessor particle-randomness)
  (particle-velocity :float :initform 1.0 :accessor particle-velocity)
  (particle-lifespan :float :initform 1.0 :accessor particle-lifespan)
  (particle-lifespan-randomness :float :initform 0.0 :accessor particle-lifespan-randomness))

(define-shader-pass particle-kickoff-pass (compute-pass)
  ((emit-threads :constant T :initform 256 :initarg :emit-threads)
   (simulate-threads :constant T :initform 256 :initarg :simulate-threads)
   (max-particles :constant T :initarg :max-particles)
   (particle-counter-buffer :buffer T :initarg :particle-counter-buffer)
   (particle-argument-buffer :buffer T :initarg :particle-argument-buffer)
   (particle-emitter-buffer :buffer T :initarg :particle-emitter-buffer))
  (:shader-file (trial "particle/kickoff.glsl")))

(define-shader-pass particle-emit-pass (compute-pass)
  ((emit-threads :constant T :initform 256 :initarg :emit-threads)
   (particle-buffer :buffer T :initarg :particle-buffer)
   (particle-emitter-buffer :buffer T :initarg :particle-emitter-buffer)
   (alive-particle-buffer-0 :buffer T :initarg :alive-particle-buffer-0)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1)
   (dead-particle-buffer :buffer T :initarg :dead-particle-buffer)
   (particle-counter-buffer :buffer T :initarg :particle-counter-buffer)
   (mesh-vertex-buffer :buffer T :initarg :mesh-vertex-buffer :accessor mesh-vertex-buffer)
   (mesh-index-buffer :buffer T :initarg :mesh-index-buffer :accessor mesh-index-buffer)
   (random-tex :port-type fixed-input :texture (// 'trial 'random)))
  (:buffers (trial standard-environment-information))
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
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "particle/simulate.glsl")))

(define-asset (trial empty-force-fields) shader-storage-block
    (make-instance 'particle-force-fields :size 1)
  :binding NIL)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shader-entity particle-emitter (standalone-shader-entity transformed-entity renderable listener)
    ((particle-emitter-buffer)
     (particle-force-fields :reader particle-force-fields)
     (particle-argument-buffer)
     (particle-counter-buffer)
     (alive-particle-buffer-0 :buffer T)
     (alive-particle-buffer-1)
     (dead-particle-buffer)
     (particle-buffer :buffer T)
     (kickoff-pass)
     (emit-pass)
     (simulate-pass)
     (texture :initform (// 'trial 'missing) :initarg :texture :accessor texture)
     (to-emit :initform 0.0 :initarg :to-emit :accessor to-emit)
     (particle-rate :initform 0.0 :initarg :particle-rate :accessor particle-rate)
     (vertex-array :initform (// 'trial 'unit-square) :accessor vertex-array)
     (max-particles :initarg :max-particles :initform 1000 :reader max-particles)
     (motion-blur :initarg :motion-blur :initform 0.0 :uniform T))
    (:buffers (trial standard-environment-information))
    (:shader-file (trial "particle/render.glsl"))))

(defmethod initialize-instance :after ((emitter particle-emitter) &key (local-threads 256) particle-force-fields vertex-array particle-options)
  (with-all-slots-bound (emitter particle-emitter)
    ;; Check that the counts are within ranges that the GPU can even compute.
    ;; On dev we error, on prod we simply clamp.
    (let ((max-threads (gl:get* :max-compute-work-group-invocations))
          (max-groups (aref (gl:get* :max-compute-work-group-count) 0)))
      #-trial-release (assert (< local-threads max-threads) (local-threads))
      #+trial-release (setf local-threads (min max-threads local-threads))
      #-trial-release (assert (< max-particles (* local-threads max-groups)) (max-particles))
      #+trial-release (setf max-particles (min max-particles (* local-threads max-groups))))
    ;; Allocate all the necessary SSBOs
    (setf particle-emitter-buffer (make-instance 'shader-storage-buffer :data-usage :dynamic-draw :binding NIL :struct (make-instance 'particle-emitter-buffer)))
    (setf particle-argument-buffer (make-instance 'shader-storage-buffer :buffer-type :draw-indirect-buffer :data-usage :dynamic-read :binding NIL :struct (make-instance 'particle-argument-buffer)))
    (setf particle-counter-buffer (make-instance 'shader-storage-buffer :data-usage :dynamic-read :binding NIL :struct (make-instance 'particle-counter-buffer :max-particles max-particles)))
    (setf alive-particle-buffer-0 (make-instance 'vertex-buffer :data-usage :dynamic-copy :binding-point T :gl-type "AliveParticles0" :element-type :unsigned-int :count max-particles))
    (setf alive-particle-buffer-1 (make-instance 'vertex-buffer :data-usage :dynamic-copy :binding-point T :gl-type "AliveParticles1" :element-type :unsigned-int :count max-particles))
    (setf dead-particle-buffer (make-instance 'vertex-buffer :data-usage :dynamic-copy :binding-point T :gl-type "DeadParticles" :element-type :unsigned-int :buffer-data
                                              (let ((buffer (make-array max-particles :element-type '(unsigned-byte 32))))
                                                (dotimes (i max-particles buffer) (setf (aref buffer i) i)))))
    (setf particle-buffer (make-instance 'shader-storage-buffer :data-usage :dynamic-copy :binding NIL :struct (make-instance 'particle-buffer :size max-particles :storage NIL)))
    ;; Allocate the compute passes
    (setf kickoff-pass (construct-delegate-object-type 'particle-kickoff-pass emitter :emit-threads local-threads :simulate-threads local-threads :max-particles max-particles :particle-counter-buffer particle-counter-buffer :particle-argument-buffer particle-argument-buffer :particle-emitter-buffer particle-emitter-buffer))
    (setf emit-pass (construct-delegate-object-type 'particle-emit-pass emitter :emit-threads local-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer :particle-emitter-buffer particle-emitter-buffer
                                                       :work-groups 0))
    (setf simulate-pass (construct-delegate-object-type 'particle-simulate-pass emitter :simulate-threads local-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer :particle-argument-buffer particle-argument-buffer
                                                               :work-groups (slot-offset 'particle-argument-buffer 'simulate-args))))
  ;; And configure our particle defaults
  (setf (vertex-array emitter) (or vertex-array (vertex-array emitter)))
  (setf (particle-options emitter) particle-options)
  (setf (particle-force-fields emitter) particle-force-fields))

(defmethod stage :after ((emitter particle-emitter) (area staging-area))
  (with-all-slots-bound (emitter particle-emitter)
    (stage kickoff-pass area)
    (stage emit-pass area)
    (stage simulate-pass area)
    (stage texture area)
    (stage vertex-array area)
    (stage (// 'trial 'empty-vertex-array) area)))

(defmethod finalize :after ((emitter particle-emitter))
  ;; FIXME: this sucks ass. We need to find a better way to ensure that gl-struct backing buffers don't leak.
  (with-all-slots-bound (emitter particle-emitter)
    (finalize particle-emitter-buffer)
    (finalize particle-force-fields)
    (finalize particle-argument-buffer)
    (finalize particle-counter-buffer)
    (finalize alive-particle-buffer-0)
    (finalize alive-particle-buffer-1)
    (finalize dead-particle-buffer)
    (finalize particle-buffer)
    (finalize kickoff-pass)
    (finalize emit-pass)
    (finalize simulate-pass)))

(defmethod (setf particle-force-fields) ((null null) (emitter particle-emitter))
  (setf (particle-force-fields emitter) (// 'trial 'empty-force-fields)))

(defmethod (setf particle-force-fields) ((cons cons) (emitter particle-emitter))
  (let* ((size (length cons))
         (struct (make-instance 'particle-force-fields :size size)))
    ;; FIXME: copy over and resize instead of this nonsense.
    (setf (slot-value struct 'particle-force-field-count) size)
    (loop for info in cons
          for i from 0
          for target = (elt (slot-value struct 'particle-force-fields) i)
          do (destructuring-bind (&key (type :point) (position (vec 0 0 0)) (strength 0.0) (range 0.0) (normal +vy3+)) info
               (setf (slot-value target 'type) (ecase type
                                                 ((NIL :none) 0)
                                                 (:point 1)
                                                 (:planet 2)
                                                 (:plane 3)
                                                 (:vortex 4)
                                                 (:sphere 5)))
               (setf (slot-value target 'position) position)
               (setf (slot-value target 'strength) strength)
               (setf (slot-value target 'range) range)
               (setf (slot-value target 'inv-range) (if (= 0.0 range) 0.0 (/ range)))
               (setf (slot-value target 'normal) normal)))
    (setf (particle-force-fields emitter) struct)))

(defmethod (setf particle-force-fields) ((struct particle-force-fields) (emitter particle-emitter))
  ;; FIXME: potentially leaky!!
  (unless (slot-boundp emitter 'particle-force-fields)
    (setf (particle-force-fields emitter) (make-instance 'shader-storage-buffer :struct NIL :binding NIL)))
  (setf (buffer-data (particle-force-fields emitter)) struct)
  (when (allocated-p (particle-force-fields emitter))
    (update-buffer-data (particle-force-fields emitter) T)))

(defmethod (setf particle-force-fields) ((buffer resource) (emitter particle-emitter))
  (setf (slot-value emitter 'particle-force-fields) buffer)
  (setf (slot-value (slot-value emitter 'simulate-pass) 'particle-force-fields) buffer))

(defmethod (setf vertex-array) ((resource placeholder-resource) (emitter particle-emitter))
  (setf (vertex-array emitter) (ensure-generated resource)))

(defmethod (setf vertex-array) :after ((vao vertex-array) (emitter particle-emitter))
  (setf (mesh-index-buffer (slot-value emitter 'emit-pass)) (or (index-buffer vao)
                                                                (error "VAO must be indexed!")))
  (setf (mesh-vertex-buffer (slot-value emitter 'emit-pass)) (first (find-if #'consp (bindings vao))))
  (with-buffer-tx (struct (slot-value emitter 'particle-emitter-buffer) :update NIL)
    (setf (mesh-index-count struct) (size vao))
    (setf (mesh-vertex-position-stride struct)
          (loop for binding in (bindings vao)
                do (when (and (listp binding) (= 0 (getf (rest binding) :index)))
                     (return (floor (getf (rest binding) :stride) (gl-type-size (element-type (first binding))))))))
    (setf (mesh-vertex-normal-stride struct)
          (loop for binding in (bindings vao)
                do (when (and (listp binding) (= 1 (getf (rest binding) :index)))
                     (return (floor (getf (rest binding) :stride) (gl-type-size (element-type (first binding))))))))))

(defmethod (setf particle-options) (options (emitter particle-emitter))
  (destructuring-bind (&key size scaling rotation color randomness velocity lifespan lifespan-randomness) options
    (when size
      (setf (particle-size emitter) size))
    (when scaling
      (setf (particle-scaling emitter) scaling))
    (when rotation
      (setf (particle-rotation emitter) rotation))
    (when color
      (setf (particle-color emitter) color))
    (when randomness
      (setf (particle-randomness emitter) randomness))
    (when velocity
      (setf (particle-velocity emitter) velocity))
    (when lifespan
      (setf (particle-lifespan emitter) lifespan))
    (when lifespan-randomness
      (setf (particle-lifespan-randomness emitter) lifespan-randomness))))

(macrolet ((define-delegate (accessor)
             `(progn (defmethod ,accessor ((emitter particle-emitter))
                       (,accessor (buffer-data (slot-value emitter 'particle-emitter-buffer))))

                     (defmethod (setf ,accessor) (value (emitter particle-emitter))
                       (setf (,accessor (buffer-data (slot-value emitter 'particle-emitter-buffer))) value)))))
  (define-delegate particle-size)
  (define-delegate particle-scaling)
  (define-delegate particle-rotation)
  (define-delegate particle-randomness)
  (define-delegate particle-velocity)
  (define-delegate particle-lifespan)
  (define-delegate particle-lifespan-randomness))

(defmethod particle-color ((emitter particle-emitter))
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (vec (/ (ldb (byte 8  0) int) 255)
         (/ (ldb (byte 8  8) int) 255)
         (/ (ldb (byte 8 16) int) 255))))

(defmethod (setf particle-color) ((color vec3) (emitter particle-emitter))
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (setf (ldb (byte 8 16) int) (clamp 0 (truncate (* (vz color) 255)) 255))
    (setf (ldb (byte 8  8) int) (clamp 0 (truncate (* (vy color) 255)) 255))
    (setf (ldb (byte 8  0) int) (clamp 0 (truncate (* (vx color) 255)) 255))
    (setf (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer))) int)))

(define-handler (particle-emitter tick) (dt)
  (with-all-slots-bound (particle-emitter particle-emitter)
    (multiple-value-bind (to-emit emit-carry) (floor (incf (to-emit particle-emitter) (* dt (particle-rate particle-emitter))))
      (with-buffer-tx (struct particle-emitter-buffer)
        (setf (transform-matrix struct) (tmat4 (tf particle-emitter)))
        (setf (emit-count struct) to-emit)
        (setf (randomness struct) (random 1.0)))
      ;; Simulate with compute shaders
      (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
      (render kickoff-pass NIL)
      (render emit-pass NIL)
      (render simulate-pass NIL)
      ;; Swap the buffers
      (rotatef (binding-point alive-particle-buffer-0)
               (binding-point alive-particle-buffer-1))
      (%gl:bind-buffer :dispatch-indirect-buffer 0)
      (setf (to-emit particle-emitter) emit-carry))))

(define-handler (particle-emitter class-changed) ()
  (handle class-changed (slot-value particle-emitter 'kickoff-pass))
  (handle class-changed (slot-value particle-emitter 'emit-pass))
  (handle class-changed (slot-value particle-emitter 'simulate-pass)))

(defmethod bind-textures ((emitter particle-emitter))
  (gl:active-texture :texture0)
  (gl:bind-texture (target (texture emitter)) (gl-name (texture emitter))))

(defmethod render ((emitter particle-emitter) (program shader-program))
  (gl:depth-mask NIL)
  (gl:blend-func :src-alpha :one)
  (gl:bind-vertex-array (gl-name (// 'trial 'empty-vertex-array)))
  (%gl:bind-buffer :draw-indirect-buffer (gl-name (slot-value emitter 'particle-argument-buffer)))
  (%gl:draw-arrays-indirect :triangles (slot-offset 'particle-argument-buffer 'draw-args))
  (gl:bind-vertex-array 0)
  (%gl:bind-buffer :draw-indirect-buffer 0)
  (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
  (gl:depth-mask T))

(defmethod emit ((particle-emitter particle-emitter) count &rest particle-options &key vertex-array location orientation scaling transform)
  ;; We do the emit **right now** so that the particle options are only active for the
  ;; current emit. Otherwise, if we wanted to emit multiple configurations in a single tick,
  ;; we'd be overwriting it until the next simulation run.
  (setf (particle-options particle-emitter) (remf* particle-options :vertex-array :location :orientation :scaling :transform))
  (when transform (setf (tf particle-emitter) transform))
  (when location (setf (location particle-emitter) location))
  (when scaling (setf (scaling particle-emitter) scaling))
  (when orientation (setf (orientation particle-emitter) orientation))
  (when vertex-array (setf (vertex-array particle-emitter) vertex-array))
  (with-all-slots-bound (particle-emitter particle-emitter)
    (with-buffer-tx (struct particle-emitter-buffer)
      (setf (emit-count struct) count)
      (setf (randomness struct) (random 1.0)))
    (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
    (render kickoff-pass NIL)
    (render emit-pass NIL)
    (%gl:bind-buffer :dispatch-indirect-buffer 0)))

(define-shader-pass depth-colliding-particle-simulate-pass (particle-simulate-pass)
  ((depth-tex :port-type fixed-input :accessor depth))
  (:shader-file (trial "particle/depth-collisions.glsl")))

(define-shader-entity depth-colliding-particle-emitter (particle-emitter)
  ())

;; KLUDGE: This is NOT great for composing stuff.
(defmethod construct-delegate-object-type ((type (eql 'particle-simulate-pass)) (emitter depth-colliding-particle-emitter) &rest args)
  (apply #'make-instance 'depth-colliding-particle-simulate-pass args))

(defmethod port ((emitter depth-colliding-particle-emitter) (name (eql 'depth)))
  (port (slot-value emitter 'simulate-pass) 'depth-tex))
