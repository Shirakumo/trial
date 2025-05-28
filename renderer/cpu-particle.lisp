(in-package #:org.shirakumo.fraf.trial)

(defstruct (raw-particle
            (:constructor make-raw-particle (&optional location velocity age))
            (:copier NIL)
            (:predicate NIL))
  (location (vec 0 0 0) :type vec3)
  (velocity (vec 0 0 0) :type vec3)
  (age 0.0 :type single-float))

(defmethod apply-force ((field particle-force-field) (particle raw-particle) dt)
  ;; FIXME: this produces a ton of garbage due to std430-refs from the field struct
  (declare (type single-float dt))
  (let ((force (vec 0 0 0))
        (location (raw-particle-location particle))
        (velocity (raw-particle-velocity particle)))
    (declare (dynamic-extent force))
    (with-gl-slots (particle-force-field type position strength range inv-range normal) field
      (case type
        (0)
        (1                              ; Point
         (let ((dir (v- position location)))
           (declare (dynamic-extent dir))
           (nv+* force dir (* strength (- 1 (clamp 0.0 (* (vlength dir) inv-range) 1.0))))))
        (2                              ; Direction
         (nv+* force normal strength))
        (3                              ; Plane
         (let ((dist (v. normal (v- location position))))
           (nv+* force normal (* strength (- 1 (clamp 0.0 (* dist inv-range) 1.0))))))
        (4                              ; Vortex
         (let* ((dir (v- location position))
                (t0 (/ (v. normal dir) (v. normal normal)))
                (dist (vdistance location (v* position t0)))
                (perp (nvunit* (vc normal dir))))
           (declare (dynamic-extent dir perp))
           (nv+* force perp (* strength (- 1 (clamp 0.0 (* dist inv-range) 1.0))))))
        (5                              ; Sphere
         (let* ((dir (v- position location))
                (dist (vlength dir)))
           (declare (dynamic-extent dir))
           (when (< dist range)
             (let* ((push (nvunit (nv- dir)))
                    (slide (nvc (vc velocity push) (nv- push))))
               (declare (dynamic-extent push slide))
               (nv+* force (nv- slide velocity) (/ dt))))))
        (6                              ; Planet
         (let* ((dir (v- position location))
                (dist (vlength dir)))
           (declare (dynamic-extent dir))
           (cond ((< dist range)
                  (let* ((push (nvunit (nv- dir)))
                         (slide (nvc (vc velocity push) (nv- push))))
                    (declare (dynamic-extent push slide))
                    (nv+* force (nv- slide velocity) (/ dt))))
                 (T
                  (nv+* force dir (/ strength (* dist dist)))))))
        (7                              ; Brake
         (!v* force velocity (- strength)))
        (8                              ; Turbulence
         (let* ((offset (mod (raw-particle-age particle) 256.0))
                (f 4.0))
           (noise:with-sample s (noise:curl/3d (varr location) f (noise:xxhash) #'noise:perlin/3d offset)
             (declare (ignore s))
             (incf (vx force) (* strength sdx))
             (incf (vy force) (* strength sdy))
             (incf (vz force) (* strength sdz)))))))
    (nv+* (raw-particle-velocity particle) force dt)))

(defun %simulate-particle (data in out dt force-fields)
  (declare (type (simple-array single-float (*)) data))
  (declare (type (unsigned-byte 32) in out))
  (declare (type single-float dt))
  (declare (optimize speed))
  (let* ((loc (vec (aref data (+ in 0))
                   (aref data (+ in 1))
                   (aref data (+ in 2))))
         (vel (vec (aref data (+ in 3))
                   (aref data (+ in 4))
                   (aref data (+ in 5))))
         (tt (aref data (+ in 6)))
         (particle (make-raw-particle loc vel tt)))
    (declare (dynamic-extent loc vel particle))
    ;; Perform simulation
    (loop with fields = (particle-force-fields force-fields)
          for i from 0 below (particle-force-field-count force-fields)
          for field = (aref fields i)
          do (apply-force field particle dt))
    (nv+* loc vel dt)
    ;; Store
    (setf (aref data (+ out 0)) (vx loc))
    (setf (aref data (+ out 1)) (vy loc))
    (setf (aref data (+ out 2)) (vz loc))
    (setf (aref data (+ out 3)) (vx vel))
    (setf (aref data (+ out 4)) (vy vel))
    (setf (aref data (+ out 5)) (vz vel))
    (prog1 (setf (aref data (+ out 6)) (- tt dt))
      (setf (aref data (+ out 7)) (aref data (+ in 7))))))

(defun %simulate-particles (particles live-particles free-list dt force-fields)
  ;; Returns updated live particle count
  (loop with out = 0
        for in from 0 below (* live-particles 8) by 8
        for life = (%simulate-particle particles in out dt force-fields)
        do (if (<= life 0)
               (vector-push (float-features:single-float-bits (aref particles (+ in 7))) free-list)
               (incf out 8))
        finally (return (truncate out 8))))

(declaim (inline random-face))
(declaim (ftype (function (simple-array &optional single-float) (values (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32) &optional))))
(defun random-face (faces &optional (random (random 1f0)))
  (declare (type (single-float 0f0) random))
  (etypecase faces
    ((simple-array (unsigned-byte 16) (*))
     (let ((tri (* 3 (truncate (* (length faces) random) 3))))
       (declare (type (unsigned-byte 32) tri))
       (values (aref faces (+ tri 0))
               (aref faces (+ tri 1))
               (aref faces (+ tri 2)))))
    ((simple-array (unsigned-byte 32) (*))
     (let ((tri (* 3 (truncate (* (length faces) random) 3))))
       (declare (type (unsigned-byte 32) tri))
       (values (aref faces (+ tri 0))
               (aref faces (+ tri 1))
               (aref faces (+ tri 2)))))))

(defun random-point-on-mesh-surface (vertex-data vertex-stride faces randoms &optional (location (vec3)) (normal (vec3)))
  (declare (type (simple-array single-float (*)) vertex-data))
  (declare (type (unsigned-byte 32) vertex-stride))
  (declare (type vec3 location normal randoms))
  (declare (optimize speed))
  (case (truncate (length vertex-data) vertex-stride)
    (0
     (v<- location 0)
     (sampling:normal 1.0 normal))
    (1
     ;; Special handling for point meshes
     (setf (vx location) (aref vertex-data 0))
     (setf (vy location) (aref vertex-data 1))
     (setf (vz location) (aref vertex-data 2))
     (cond ((< (length vertex-data) 6)
            (sampling:normal 1.0 normal))
           (T
            (setf (vx normal) (aref vertex-data 3))
            (setf (vy normal) (aref vertex-data 4))
            (setf (vz normal) (aref vertex-data 5)))))
    (2
     ;; Special handling for line meshes
     (let ((lerp (vx randoms)))
       (setf (vx location) (lerp (aref vertex-data 0) (aref vertex-data (+ 0 vertex-stride)) lerp))
       (setf (vy location) (lerp (aref vertex-data 1) (aref vertex-data (+ 1 vertex-stride)) lerp))
       (setf (vz location) (lerp (aref vertex-data 2) (aref vertex-data (+ 2 vertex-stride)) lerp))
       (setf (vx normal) (lerp (aref vertex-data 3) (aref vertex-data (+ 3 vertex-stride)) lerp))
       (setf (vy normal) (lerp (aref vertex-data 4) (aref vertex-data (+ 4 vertex-stride)) lerp))
       (setf (vz normal) (lerp (aref vertex-data 5) (aref vertex-data (+ 5 vertex-stride)) lerp))))
    (T
     ;; Pick a random triangle and read out the properties
     (multiple-value-bind (i0 i1 i2) (random-face faces (vz randoms))
       (let* ((i0 (* vertex-stride i0))
              (i1 (* vertex-stride i1))
              (i2 (* vertex-stride i2))
              (p0 (vec (aref vertex-data (+ i0 0)) (aref vertex-data (+ i0 1)) (aref vertex-data (+ i0 2))))
              (p1 (vec (aref vertex-data (+ i1 0)) (aref vertex-data (+ i1 1)) (aref vertex-data (+ i1 2))))
              (p2 (vec (aref vertex-data (+ i2 0)) (aref vertex-data (+ i2 1)) (aref vertex-data (+ i2 2))))
              (n0 (vec (aref vertex-data (+ i0 3)) (aref vertex-data (+ i0 4)) (aref vertex-data (+ i0 5))))
              (n1 (vec (aref vertex-data (+ i1 3)) (aref vertex-data (+ i1 4)) (aref vertex-data (+ i1 5))))
              (n2 (vec (aref vertex-data (+ i2 3)) (aref vertex-data (+ i2 4)) (aref vertex-data (+ i2 5))))
              (f (vx randoms))
              (g (vy randoms)))
         (declare (dynamic-extent p0 p1 p2 n0 n1 n2))
         (declare (type single-float f g))
         ;; Pick a random location on the triangle via barycentric interpolation
         (when (< 1 (+ f g))
           (setf f (- 1.0 f) g (- 1.0 g)))
         (flet ((eval-barycentric (target a b c)
                  (let ((ba (v- b a))
                        (ca (v- c a)))
                    (declare (dynamic-extent ba ca))
                    (v<- target a)
                    (nv+* target ba f)
                    (nv+* target ca g))))
           (eval-barycentric location p0 p1 p2)
           (eval-barycentric normal n0 n1 n2)
           (values location (nvunit normal))))))))

(defun %emit-particle (particles properties pos prop randoms randomness lifespan-randomness
                       emission-matrix system-matrix velocity rotation lifespan size scaling color
                       vertex-data vertex-stride faces)
  (declare (type (simple-array single-float (*)) particles properties vertex-data))
  (declare (type (unsigned-byte 32) pos prop color))
  (declare (type single-float lifespan lifespan-randomness randomness size scaling rotation velocity))
  (declare (type vec3 randoms))
  (declare (type mat4 emission-matrix system-matrix))
  (declare (optimize speed (safety 0)))
  (let* ((location (vec 0 0 0))
         (normal (vec 0 0 0))
         (velocity (vec velocity velocity velocity))
         (system-matrix (mtranspose system-matrix)))
    (declare (dynamic-extent location normal velocity system-matrix))
    ;; Evaluate static particle properties first
    (random-point-on-mesh-surface vertex-data vertex-stride faces randoms location normal)
    (n*m4/3 emission-matrix normal)
    (n*m emission-matrix location)
    (nv* velocity (nv+ (v* (v- randoms 0.5) randomness) normal))
    (setf (aref properties (+ prop 0)) (+ lifespan (* lifespan lifespan-randomness (- (vx randoms) 0.5))))
    (setf (aref properties (+ prop 1)) (* rotation randomness (- (vz randoms) 0.5)))
    (setf (aref properties (+ prop 2)) (+ size (* size randomness (- (vy randoms) 0.5))))
    (setf (aref properties (+ prop 3)) (* (aref properties (+ prop 2)) scaling))
    ;; Randomise flip if activated
    (when (logbitp 30 color) (setf (ldb (byte 1 30) color) (round (the (single-float 0.0 1.0) (vx randoms)))))
    (when (logbitp 31 color) (setf (ldb (byte 1 31) color) (round (the (single-float 0.0 1.0) (vy randoms)))))
    (setf (aref properties (+ prop 4)) (float-features:bits-single-float color))
    ;; We pad the matrix to offset 8 to get things vec4 aligned.
    (setf (aref properties (+ prop 5)) 0.0)
    (setf (aref properties (+ prop 6)) 0.0)
    (setf (aref properties (+ prop 7)) 0.0)
    (replace properties (marr4 system-matrix) :start1 (+ prop 8))
    ;; Now set dynamic particle properties
    (setf (aref particles (+ pos 0)) (vx location))
    (setf (aref particles (+ pos 1)) (vy location))
    (setf (aref particles (+ pos 2)) (vz location))
    (setf (aref particles (+ pos 3)) (vx velocity))
    (setf (aref particles (+ pos 4)) (vy velocity))
    (setf (aref particles (+ pos 5)) (vz velocity))
    (setf (aref particles (+ pos 6)) (aref properties (+ prop 0)))
    (setf (aref particles (+ pos 7)) (float-features:bits-single-float prop))))

(defun %allocate-particle-data (max-particles)
  (let ((particles (make-array (* max-particles 8) :element-type 'single-float))
        (properties (make-array (* max-particles 24) :element-type 'single-float))
        (free-list (make-array max-particles :element-type '(unsigned-byte 32) :fill-pointer 0)))
    (dotimes (i max-particles (values particles properties free-list))
      (vector-push (- (* (1- max-particles) 24) (* i 24)) free-list))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shader-entity cpu-particle-emitter (particle-emitter)
    (particles
     properties
     free-list
     particle-buffer
     particle-property-buffer
     face-data
     vertex-data
     vertex-stride
     (particle-force-fields :accessor particle-force-fields)
     (live-particles :initform 0 :accessor live-particles)
     (draw-vertex-array :initform NIL :accessor draw-vertex-array)
     (particle-size :initform 1.0 :accessor particle-size)
     (particle-scaling :initform 1.0 :accessor particle-scaling)
     (particle-rotation :initform 1.0 :accessor particle-rotation)
     (particle-randomness :initform 0.0 :accessor particle-randomness)
     (particle-velocity :initform 1.0 :accessor particle-velocity)
     (particle-lifespan :initform 1.0 :accessor particle-lifespan)
     (particle-lifespan-randomness :initform 0.0 :accessor particle-lifespan-randomness)
     (particle-full-color :initform #xFFFFFF :accessor particle-full-color))
    (:buffers (trial standard-environment-information))
    (:shader-file (trial "particle/cpu-render.glsl"))))

(defmethod initialize-instance :after ((emitter cpu-particle-emitter) &key)
  (multiple-value-bind (particles properties free-list) (%allocate-particle-data (max-particles emitter))
    (setf (slot-value emitter 'particles) particles)
    (setf (slot-value emitter 'properties) properties)
    (setf (slot-value emitter 'free-list) free-list)
    (setf (slot-value emitter 'particle-buffer)
          (make-instance 'vertex-buffer :data-usage :stream-draw :element-type :float :buffer-data particles))
    (setf (draw-vertex-array emitter)
          (make-instance 'vertex-array :bindings (compute-buffer-bindings (slot-value emitter 'particle-buffer)
                                                                          '((3 :float 1) (3 :float 1) (1 :float 1) (1 :float 1)))))
    (setf (slot-value emitter 'particle-property-buffer)
          (make-instance 'texture :width (truncate (length properties) 4) :height 1 :target :texture-1d :min-filter :nearest :mag-filter :nearest
                                  :internal-format :rgba32f :pixel-data properties :pixel-format :rgba :pixel-type :float))))

(defmethod stage :after ((emitter cpu-particle-emitter) (area staging-area))
  (stage (draw-vertex-array emitter) area)
  (stage (slot-value emitter 'particle-property-buffer) area))

(defmethod (setf particle-force-fields) ((null null) (emitter cpu-particle-emitter))
  (setf (particle-force-fields emitter) (make-instance 'particle-force-fields :size 0)))

(defmethod (setf mesh-index-buffer) (buffer (emitter cpu-particle-emitter))
  (setf (slot-value emitter 'face-data) (buffer-data buffer)))

(defmethod (setf mesh-vertex-buffer) (buffer (emitter cpu-particle-emitter))
  (setf (slot-value emitter 'vertex-data) (buffer-data buffer)))

(defmethod (setf mesh-vertex-stride) (stride (emitter cpu-particle-emitter))
  (setf (slot-value emitter 'vertex-stride) stride))

(define-handler ((emitter cpu-particle-emitter) tick) (dt)
  (with-all-slots-bound (emitter cpu-particle-emitter)
    (multiple-value-bind (to-emit emit-carry) (floor (incf (to-emit emitter) (* dt (particle-rate emitter))))
      (when (< 0 to-emit) (emit emitter to-emit))
      (setf (to-emit emitter) emit-carry)
      (when (< 0 live-particles)
        (setf live-particles (%simulate-particles particles live-particles free-list dt particle-force-fields))
        (update-buffer-data particle-buffer T :count (* live-particles 8 4))))))

(defmethod emit ((emitter cpu-particle-emitter) count &rest particle-options &key vertex-array location orientation scaling transform &allow-other-keys)
  (setf (particle-options emitter) (remf* particle-options :vertex-array :location :orientation :scaling :transform))
  (let ((local (transform)))
    (declare (dynamic-extent local))
    (t<- local (the transform (local-transform emitter)))
    (when location (v<- (tlocation local) location))
    (when scaling (v<- (tscaling local) scaling))
    (when orientation (q<- (trotation local) orientation))
    (when transform (t<- local transform))
    ;; FIXME: don't permanently change emitter VAO
    (when vertex-array (setf (vertex-array emitter) vertex-array))
    (with-all-slots-bound (emitter cpu-particle-emitter)
      (let ((system-matrix (mat4))
            (emission-matrix (mat4))
            (min-prop most-positive-fixnum)
            (max-prop 0))
        (declare (dynamic-extent system-matrix emission-matrix))
        (global-transform-matrix emitter system-matrix)
        (!tmat emission-matrix local)
        (dotimes (i (min count (length free-list)))
          (let ((pos (* 8 live-particles))
                (prop (vector-pop free-list)))
            (when (< prop min-prop) (setf min-prop prop))
            (when (< max-prop prop) (setf max-prop prop))
            (%emit-particle particles properties pos prop (vrand (vec 0.5 0.5 0.5) (vec3 1))
                            particle-randomness particle-lifespan-randomness emission-matrix system-matrix
                            particle-velocity particle-rotation particle-lifespan
                            particle-size particle-scaling particle-full-color
                            vertex-data vertex-stride face-data)
            (incf live-particles)))
        (when (and (< 0 live-particles) (< min-prop most-positive-fixnum))
          (let ((src (first (sources particle-property-buffer))))
            (setf (nth 0 (texture-source-src src)) (truncate min-prop 4))
            (setf (nth 3 (texture-source-dst src)) (truncate (- (+ 24 max-prop) min-prop) 4))
            (activate particle-property-buffer)
            (upload-texture-source src particle-property-buffer)))))))

(defmethod clear ((emitter cpu-particle-emitter))
  (setf (live-particles emitter) 0)
  (let ((free-list (slot-value emitter 'free-list)))
    (setf (fill-pointer free-list) 0)
    (dotimes (i (max-particles emitter))
      (vector-push (* i 21) free-list))))

(defmethod bind-textures ((emitter cpu-particle-emitter))
  (bind (texture emitter) :texture0)
  (bind (slot-value emitter 'particle-property-buffer) :texture1))

(defmethod render :around ((emitter cpu-particle-emitter) (program shader-program))
  (when (< 0 (live-particles emitter))
    (call-next-method)))

(defmethod render ((emitter cpu-particle-emitter) (program shader-program))
  (setf (uniform program "particle_data") 1)
  (with-pushed-features
    (disable-feature :cull-face)
    (with-depth-mask NIL
      (set-blend-mode (blend-mode emitter))
      (render-array (draw-vertex-array emitter) :vertex-count 6 :instances (live-particles emitter))
      (set-blend-mode :normal))))

;; FIXME: implement sorting?

(define-shader-entity multi-texture-cpu-particle-emitter (cpu-particle-emitter)
  ()
  (:inhibit-shaders (cpu-particle-emitter :fragment-shader))
  (:shader-file (trial "particle/multi-render.glsl")))
