(in-package #:org.shirakumo.fraf.trial)

;;;;; IMPLEMENTATION NOTES:
;;; This particle system is based on the one presented in [1], as
;;; part of the "Wicked Engine", specifically at this commit [2].
;;; Our implementation does not differ greatly from theirs, except
;;; to accommodate various trial-isms. According to my very basic
;;; tests, we can run around 100'000 particles with this on an old
;;; Nvidia 1050 Ti with nothing else going on. Going higher starts
;;; to dip into the frames, especially while recording.  Not sure
;;; if it's worth optimising this even further, though, as the
;;; slowness is caused by excessive overdraw, not the simulation
;;; itself. In circumstances that are less degenerate, this would
;;; not be a bottleneck.
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
;;; Special notes about the COLOR mask in the particle field:
;;; this mask is both used for the multiplicative particle color
;;; and for a bit mask, which is stored in the upper 8 bits of the
;;; integer. The 8 bits are assigned as follows:
;;;
;;; 0x01 24 --- [ sprite selector ]
;;; 0x02 25 --- [ sprite selector ]
;;; 0x04 26 --- [ sprite selector ]
;;; 0x08 27 --- 
;;; 0x10 28 --- 
;;; 0x20 29 --- rectangle rendering mode
;;; 0x40 30 --- vertical flip (randomised)
;;; 0x80 31 --- horizontal flip (randomised)
;;;
;;; [1] https://wickedengine.net/2017/11/07/gpu-based-particle-simulation/
;;; [2] https://github.com/turanszkij/WickedEngine/tree/9caf25a52996c6c62fc39f10784d8951f715b05d/WickedEngine

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
  (real-emit-count :uint :initform 0))

(define-gl-struct (particle-argument-buffer :layout-standard std140)
  (emit-args :uvec3 :initform (vec 0 0 0))
  (simulate-args :uvec3 :initform (vec 0 0 0))
  (draw-args :uvec4 :initform (vec 0 0 0 0)))

(define-gl-struct (particle-emitter-buffer :layout-standard std430)
  (model-matrix :mat4 :accessor transform-matrix)
  (emit-count :uint :initform 0 :accessor emit-count)
  (mesh-index-count :uint :initform 0 :accessor mesh-index-count)
  (mesh-vertex-stride :uint :initform 0 :accessor mesh-vertex-stride)
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
   (random-tex :port-type fixed-input :texture (// 'trial 'random))
   (barrier :initform :shader-storage-barrier))
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
   (particle-argument-buffer :buffer T :initarg :particle-argument-buffer)
   (barrier :initform :shader-storage-barrier))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "particle/simulate.glsl")))

(define-asset (trial empty-force-fields) shader-storage-block
    (make-instance 'particle-force-fields :size 1)
  :binding NIL)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shader-entity gpu-particle-emitter (particle-emitter)
    ((particle-emitter-buffer)
     (particle-argument-buffer)
     (particle-counter-buffer)
     (particle-force-fields-buffer :accessor particle-force-fields-buffer)
     (alive-particle-buffer-0 :buffer T)
     (alive-particle-buffer-1)
     (dead-particle-buffer)
     (particle-buffer :buffer T)
     (kickoff-pass)
     (emit-pass)
     (simulate-pass)
     (local-threads :initarg :local-threads :initform 256 :reader local-threads))
    (:shader-file (trial "particle/gpu-render.glsl"))))

(defmethod initialize-instance :after ((emitter gpu-particle-emitter) &key)
  (with-all-slots-bound (emitter gpu-particle-emitter)
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
    (setf particle-buffer (make-instance 'shader-storage-buffer :data-usage :dynamic-copy :binding NIL :struct (make-instance 'particle-buffer :size max-particles :storage T)))
    ;; Allocate the compute passes
    (setf kickoff-pass (construct-delegate-object-type 'particle-kickoff-pass emitter :emit-threads local-threads :simulate-threads local-threads :max-particles max-particles :particle-counter-buffer particle-counter-buffer :particle-argument-buffer particle-argument-buffer :particle-emitter-buffer particle-emitter-buffer))
    (setf emit-pass (construct-delegate-object-type 'particle-emit-pass emitter :emit-threads local-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer :particle-emitter-buffer particle-emitter-buffer
                                                       :work-groups 0))
    (setf simulate-pass (construct-delegate-object-type 'particle-simulate-pass emitter :simulate-threads local-threads :particle-buffer particle-buffer :alive-particle-buffer-0 alive-particle-buffer-0 :alive-particle-buffer-1 alive-particle-buffer-1 :dead-particle-buffer dead-particle-buffer :particle-counter-buffer particle-counter-buffer :particle-argument-buffer particle-argument-buffer
                                                               :work-groups (slot-offset 'particle-argument-buffer 'simulate-args)))))

(defmethod stage :after ((emitter gpu-particle-emitter) (area staging-area))
  (with-all-slots-bound (emitter gpu-particle-emitter)
    (stage kickoff-pass area)
    (stage emit-pass area)
    (stage simulate-pass area)))

(defmethod finalize :after ((emitter gpu-particle-emitter))
  ;; FIXME: this sucks ass. We need to find a better way to ensure that gl-struct backing buffers don't leak.
  (with-all-slots-bound (emitter gpu-particle-emitter)
    (finalize particle-emitter-buffer)
    (finalize particle-force-fields-buffer)
    (finalize particle-argument-buffer)
    (finalize particle-counter-buffer)
    (finalize alive-particle-buffer-0)
    (finalize alive-particle-buffer-1)
    (finalize dead-particle-buffer)
    (finalize particle-buffer)
    (finalize kickoff-pass)
    (finalize emit-pass)
    (finalize simulate-pass)))

(defmethod (setf particle-force-fields) ((null null) (emitter gpu-particle-emitter))
  (setf (particle-force-fields emitter) (// 'trial 'empty-force-fields)))

(defmethod (setf particle-force-fields) :after ((cons cons) (emitter gpu-particle-emitter))
  (when (allocated-p (particle-force-fields-buffer emitter))
    (update-buffer-data (particle-force-fields-buffer emitter) T)))

(defmethod (setf particle-force-fields) ((struct particle-force-fields) (emitter gpu-particle-emitter))
  (cond ((not (slot-boundp emitter 'particle-force-fields-buffer))
         (setf (particle-force-fields emitter) (make-instance 'shader-storage-buffer :struct struct :binding NIL)))
        ;; FIXME: potentially leaky!!
        ;; IF the buffer wasn't generated then we probably constructed it and thus own the data.
        ;; Free it here since we're replacing it.
        ((not (generator (particle-force-fields-buffer emitter)))
         (unless (eq struct (buffer-data (particle-force-fields-buffer emitter)))
           (finalize (buffer-data (particle-force-fields-buffer emitter))))))
  (setf (buffer-data (particle-force-fields-buffer emitter)) struct)
  (when (allocated-p (particle-force-fields-buffer emitter))
    (resize-buffer-data (particle-force-fields-buffer emitter) T)))

(defmethod (setf particle-force-fields) ((buffer resource) (emitter gpu-particle-emitter))
  (setf (slot-value emitter 'particle-force-fields-buffer) buffer)
  (setf (slot-value (slot-value emitter 'simulate-pass) 'particle-force-fields) buffer))

(defmethod particle-force-fields ((emitter gpu-particle-emitter))
  (buffer-data (particle-force-fields-buffer emitter)))

(defmethod (setf mesh-index-buffer) (buffer (emitter gpu-particle-emitter))
  (unless (member (element-type buffer) '(:int :unsigned-int))
    (setf (element-type buffer) :unsigned-int)
    (setf (buffer-data buffer) (make-array (length (buffer-data buffer))
                                           :element-type '(unsigned-byte 32)
                                           :initial-contents (buffer-data buffer))))
  (setf (mesh-index-buffer (slot-value emitter 'emit-pass)) buffer))

(defmethod (setf mesh-vertex-buffer) (buffer (emitter gpu-particle-emitter))
  (setf (mesh-vertex-buffer (slot-value emitter 'emit-pass)) buffer))

(defmethod (setf mesh-vertex-stride) (stride (emitter gpu-particle-emitter))
  (with-buffer-tx (struct (slot-value emitter 'particle-emitter-buffer) :update NIL)
    (setf (mesh-index-count struct) (size (vertex-array emitter)))
    (setf (mesh-vertex-stride struct) stride)))

(defmethod live-particles ((emitter gpu-particle-emitter))
  (with-buffer-tx (struct (slot-value emitter 'trial::particle-counter-buffer) :update :read)
    (slot-value struct 'trial::alive-count)))

(macrolet ((define-delegate (accessor &optional (inner accessor))
             `(progn (defmethod ,accessor ((emitter gpu-particle-emitter))
                       (,inner (buffer-data (slot-value emitter 'particle-emitter-buffer))))

                     (defmethod (setf ,accessor) (value (emitter gpu-particle-emitter))
                       (setf (,inner (buffer-data (slot-value emitter 'particle-emitter-buffer))) value)))))
  (define-delegate particle-size)
  (define-delegate particle-scaling)
  (define-delegate particle-rotation)
  (define-delegate particle-randomness)
  (define-delegate particle-velocity)
  (define-delegate particle-lifespan)
  (define-delegate particle-lifespan-randomness)
  (define-delegate particle-full-color particle-color))

(defmethod simulate-particles ((emitter gpu-particle-emitter))
  (render (slot-value emitter 'simulate-pass) NIL))

(define-handler ((emitter gpu-particle-emitter) tick) (dt)
  (with-all-slots-bound (emitter gpu-particle-emitter)
    (multiple-value-bind (to-emit emit-carry) (floor (incf to-emit (* dt particle-rate)))
      (with-buffer-tx (struct particle-emitter-buffer)
        (setf (transform-matrix struct) (tmat (tf emitter)))
        (setf (emit-count struct) to-emit)
        (setf (randomness struct) (random 1.0)))
      ;; Simulate with compute shaders
      (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
      (render kickoff-pass NIL)
      (render emit-pass NIL)
      (simulate-particles emitter)
      ;; Swap the buffers
      (rotatef (binding-point alive-particle-buffer-0)
               (binding-point alive-particle-buffer-1))
      (%gl:bind-buffer :dispatch-indirect-buffer 0)
      (setf (to-emit emitter) emit-carry))))

(define-handler (gpu-particle-emitter class-changed) ()
  (handle class-changed (slot-value gpu-particle-emitter 'kickoff-pass))
  (handle class-changed (slot-value gpu-particle-emitter 'emit-pass))
  (handle class-changed (slot-value gpu-particle-emitter 'simulate-pass)))

(defmethod clear ((emitter gpu-particle-emitter))
  (with-buffer-tx (struct (slot-value emitter 'particle-counter-buffer) :update :write)
    (setf (slot-value struct 'dead-count) (max-particles emitter))))

(defmethod bind-textures ((emitter gpu-particle-emitter))
  (bind (texture emitter) :texture0))

(defmethod render :before ((emitter gpu-particle-emitter) (program shader-program))
  (setf (uniform program "model_matrix") (tmat (tf emitter)))
  (%gl:bind-buffer :draw-indirect-buffer (gl-name (slot-value emitter 'particle-argument-buffer)))
  (activate (// 'trial 'empty-vertex-array)))

(defmethod render :after ((emitter gpu-particle-emitter) (program shader-program))
  (deactivate (// 'trial 'empty-vertex-array))
  (%gl:bind-buffer :draw-indirect-buffer 0))

(defmethod render ((emitter gpu-particle-emitter) (program shader-program))
  (with-depth-mask NIL
    (set-blend-mode (blend-mode emitter))
    (%gl:draw-arrays-indirect :triangles (slot-offset 'particle-argument-buffer 'draw-args))
    (set-blend-mode :normal)))

(defmethod emit ((particle-emitter gpu-particle-emitter) count &rest particle-options &key vertex-array location orientation scaling transform)
  ;; We do the emit **right now** so that the particle options are only active for the
  ;; current emit. Otherwise, if we wanted to emit multiple configurations in a single tick,
  ;; we'd be overwriting it until the next simulation run.
  (setf (particle-options particle-emitter) (remf* particle-options :vertex-array :location :orientation :scaling :transform))
  (when transform (setf (tf particle-emitter) transform))
  (when location (setf (location particle-emitter) location))
  (when scaling (setf (scaling particle-emitter) scaling))
  (when orientation (setf (orientation particle-emitter) orientation))
  (when vertex-array (setf (vertex-array particle-emitter) vertex-array))
  (with-all-slots-bound (particle-emitter gpu-particle-emitter)
    (with-buffer-tx (struct particle-emitter-buffer)
      (setf (emit-count struct) count)
      (setf (randomness struct) (random 1.0)))
    (%gl:bind-buffer :dispatch-indirect-buffer (gl-name particle-argument-buffer))
    (render kickoff-pass NIL)
    (render emit-pass NIL)
    (%gl:bind-buffer :dispatch-indirect-buffer 0)))

(define-shader-pass depth-colliding-particle-simulate-pass (particle-simulate-pass)
  ((depth-tex :port-type fixed-input :accessor depth)
   (surface-thickness :uniform T :initform 1.5 :accessor surface-thickness))
  (:shader-file (trial "particle/depth-collisions.glsl")))

(define-shader-entity depth-colliding-particle-emitter (gpu-particle-emitter)
  ())

;; KLUDGE: This is NOT great for composing stuff.
(defmethod construct-delegate-object-type ((type (eql 'particle-simulate-pass)) (emitter depth-colliding-particle-emitter) &rest args)
  (apply #'make-instance 'depth-colliding-particle-simulate-pass args))

(defmethod port ((emitter depth-colliding-particle-emitter) (name (eql 'depth)))
  (port (slot-value emitter 'simulate-pass) 'depth-tex))

(defmethod connect ((pass standard-render-pass) (emitter depth-colliding-particle-emitter) scene)
  (flow:connect (port pass 'depth) (port emitter 'depth) 'flow:directed-connection)
  (enter pass scene))

(defmethod surface-thickness ((emitter depth-colliding-particle-emitter))
  (surface-thickness (slot-value emitter 'simulate-pass)))

(defmethod (setf surface-thickness) (value (emitter depth-colliding-particle-emitter))
  (setf (surface-thickness (slot-value emitter 'simulate-pass)) (float value 0f0)))

(define-shader-pass particle-sort-pass (compute-pass)
  ((sort-size :initform 512 :constant T :initarg :sort-size)
   (elements :initform 0 :uniform T)
   (particle-distances :buffer T :initarg :particle-distances)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1))
  (:shader-file (trial "particle/sort.glsl")))

(define-shader-pass particle-sort-step-pass (compute-pass)
  ((sort-size :initform 512 :constant T :initarg :sort-size)
   (elements :initform 0 :uniform T)
   (particle-distances :buffer T :initarg :particle-distances)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1))
  (:shader-file (trial "particle/sort-step.glsl")))

(define-shader-pass particle-sort-inner-pass (compute-pass)
  ((sort-size :initform 512 :constant T :initarg :sort-size)
   (elements :initform 0 :uniform T)
   (particle-distances :buffer T :initarg :particle-distances)
   (alive-particle-buffer-1 :buffer T :initarg :alive-particle-buffer-1))
  (:shader-file (trial "particle/sort-inner.glsl")))

(define-shader-pass sorted-particle-simulate-pass (particle-simulate-pass)
  ((particle-distances :buffer T :initarg :particle-distances))
  (:shader-file (trial "particle/sort-simulate.glsl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shader-entity sorted-particle-emitter (gpu-particle-emitter)
    (particle-distances
     sort-pass
     sort-step-pass
     sort-inner-pass)))

(defmethod initialize-instance :after ((emitter sorted-particle-emitter) &key)
  (with-all-slots-bound (emitter sorted-particle-emitter)
    (setf sort-pass (construct-delegate-object-type 'particle-sort-pass emitter :particle-distances particle-distances :alive-particle-buffer-1 alive-particle-buffer-1))
    (setf sort-step-pass (construct-delegate-object-type 'particle-sort-step-pass emitter :particle-distances particle-distances :alive-particle-buffer-1 alive-particle-buffer-1))
    (setf sort-inner-pass (construct-delegate-object-type 'particle-sort-inner-pass emitter :particle-distances particle-distances :alive-particle-buffer-1 alive-particle-buffer-1))))

(defmethod construct-delegate-object-type ((type (eql 'particle-simulate-pass)) (emitter sorted-particle-emitter) &rest args)
  (let ((buffer (make-instance 'vertex-buffer :data-usage :dynamic-copy :binding-point T :gl-type "ParticleDistances" :element-type :float :count (max-particles emitter))))
    (setf (slot-value emitter 'particle-distances) buffer)
    (apply #'make-instance 'sorted-particle-simulate-pass :particle-distances buffer args)))

(defmethod stage :after ((emitter sorted-particle-emitter) (area staging-area))
  (with-all-slots-bound (emitter sorted-particle-emitter)
    (stage sort-pass area)
    (stage sort-step-pass area)
    (stage sort-inner-pass area)))

(defmethod finalize :after ((emitter sorted-particle-emitter))
  (with-all-slots-bound (emitter sorted-particle-emitter)
    (finalize sort-pass)
    (finalize sort-step-pass)
    (finalize sort-inner-pass)))

(defmethod simulate-particles :after ((emitter sorted-particle-emitter))
  (with-all-slots-bound (emitter sorted-particle-emitter)
    (let* ((alive (with-buffer-tx (struct particle-counter-buffer :update :read)
                    (- (max-particles emitter) (slot-value struct 'dead-count))))
           (thread-groups (* 2 (ceiling alive 512)))
           (sorted 512))
      (setf (slot-value sort-pass 'elements) alive)
      (render sort-pass (vec (ash thread-groups -1) 1 1))
      (when (< sorted alive)
        (setf (slot-value sort-step-pass 'elements) alive)
        (setf (slot-value sort-inner-pass 'elements) alive)
        (loop do (activate (shader-program sort-step-pass))
                 (loop for sub-size = sorted then (ash sub-size -1)
                       while (< 256 sub-size)
                       do (if (= sub-size sorted)
                              ;; KLUDGE: we don't have native ivecs yet, so....
                              (%gl:uniform-4i (uniform-location (shader-program sort-step-pass) "job_params")
                                              sub-size (1- (* sub-size 2)) -1 0)
                              (%gl:uniform-4i (uniform-location (shader-program sort-step-pass) "job_params")
                                              sub-size sub-size +1 0))
                          (render sort-step-pass (vec thread-groups 1 1)))
                 (render sort-inner-pass (vec thread-groups 1 1))
                 (setf sorted (* sorted 2))
              until (< alive sorted))))))

(defmethod render ((emitter sorted-particle-emitter) (program shader-program))
  (with-depth-mask NIL
    (%gl:draw-arrays-indirect :triangles (slot-offset 'particle-argument-buffer 'draw-args))))

(define-shader-entity multi-texture-particle-emitter (gpu-particle-emitter)
  ()
  (:shader-file (trial "particle/multi-render.glsl")))

