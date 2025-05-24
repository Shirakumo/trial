(in-package #:org.shirakumo.fraf.trial)

(defun map-particle-force-field-type (type)
  (case type
    ((0 NIL :none) 0)
    ((1 :point) 1)
    ((2 :direction) 2)
    ((3 :plane) 3)
    ((4 :vortex) 4)
    ((5 :sphere) 5)
    ((6 :planet) 6)
    ((7 :brake) 7)
    (T type)))

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

(define-shader-entity particle-emitter (standalone-shader-entity transformed-entity renderable listener)
  ((texture :initform (// 'trial 'missing) :initarg :texture :accessor texture)
   (to-emit :initform 0.0 :initarg :to-emit :accessor to-emit)
   (particle-rate :initform 0.0 :initarg :particle-rate :accessor particle-rate)
   (vertex-array :initform (// 'trial 'unit-square) :accessor vertex-array)
   (max-particles :initarg :max-particles :initform 1000 :reader max-particles)
   (motion-blur :initarg :motion-blur :initform 0.0 :uniform T :accessor particle-motion-blur)
   (color-multiplier :initarg :color-multiplier :initform (vec4 1) :uniform T :accessor particle-color-multiplier)
   (offset :initarg :offset :initform (vec3 0) :uniform T :accessor particle-offset)
   (particle-force-fields :reader particle-force-fields)
   (blend-mode :initarg :blend-mode :initform :add :accessor blend-mode))
  (:buffers (trial standard-environment-information)))

(defmethod initialize-instance :around ((emitter particle-emitter) &key particle-options particle-force-fields vertex-array)
  (call-next-method)
  (setf (vertex-array emitter) (or vertex-array (vertex-array emitter)))
  (setf (particle-options emitter) particle-options)
  (setf (particle-force-fields emitter) particle-force-fields))

(defmethod stage :after ((emitter particle-emitter) (area staging-area))
  (stage (texture emitter) area)
  (stage (vertex-array emitter) area)
  (stage (// 'trial 'empty-vertex-array) area))

(defmethod (setf particle-options) (options (emitter particle-emitter))
  (destructuring-bind (&key texture size scaling rotation color randomness velocity lifespan lifespan-randomness sprite (flip NIL flip-t) mode &allow-other-keys) options
    (when texture
      (setf (texture emitter) texture))
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
      (setf (particle-lifespan-randomness emitter) lifespan-randomness))
    (when sprite
      (setf (particle-sprite emitter) sprite))
    (when flip-t
      (setf (particle-flip emitter) flip))
    (when mode
      (setf (particle-mode emitter) mode))))

(defmethod particle-mode ((emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (if (logbitp 29 int) :quad :billboard)))

(defmethod (setf particle-mode) (mode (emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (setf (ldb (byte 1 29) int) (ecase mode
                                  (:quad 1)
                                  (:billboard 0)))
    (setf (particle-full-color emitter) int)
    mode))

(defmethod particle-flip ((emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (case (ldb (byte 2 30) int)
      (0 NIL)
      (1 :y)
      (2 :x)
      (3 T))))

(defmethod (setf particle-flip) (flip (emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (setf (ldb (byte 2 30) int) (ecase flip
                                  ((NIL) 0)
                                  (:y 1)
                                  (:x 2)
                                  ((T) 3)))
    (setf (particle-full-color emitter) int)
    flip))

(defmethod particle-color ((emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (vec (/ (ldb (byte 8  0) int) 255)
         (/ (ldb (byte 8  8) int) 255)
         (/ (ldb (byte 8 16) int) 255))))

(defmethod (setf particle-color) ((color vec3) (emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (setf (ldb (byte 8 16) int) (clamp 0 (truncate (* (vz color) 255)) 255))
    (setf (ldb (byte 8  8) int) (clamp 0 (truncate (* (vy color) 255)) 255))
    (setf (ldb (byte 8  0) int) (clamp 0 (truncate (* (vx color) 255)) 255))
    (setf (particle-full-color emitter) int)
    color))

(defmethod (setf particle-color) ((color vec4) (emitter particle-emitter))
  (let ((int (particle-full-color emitter)))
    (setf (ldb (byte 8 16) int) (clamp 0 (truncate (* (vz color) 255)) 255))
    (setf (ldb (byte 8  8) int) (clamp 0 (truncate (* (vy color) 255)) 255))
    (setf (ldb (byte 8  0) int) (clamp 0 (truncate (* (vx color) 255)) 255))
    (setf (particle-full-color emitter) int)
    color))

(defmethod particle-sprite ((emitter particle-emitter))
  (let* ((int (particle-full-color emitter))
         (sprite (ldb (byte 3 24) int)))
    (if (= #b111 sprite) :random sprite)))

(defmethod (setf particle-sprite) (sprite (emitter particle-emitter))
  (check-type sprite (unsigned-byte 3))
  (let ((int (particle-full-color emitter)))
    (setf (ldb (byte 3 24) int) sprite)
    (setf (particle-full-color emitter) int)
    sprite))

(defmethod (setf particle-sprite) ((sprite (eql :random)) (emitter particle-emitter))
  (setf (particle-sprite emitter) #b111))

(defmethod (setf vertex-array) ((resource placeholder-resource) (emitter particle-emitter))
  (setf (vertex-array emitter) (ensure-generated resource)))

(defmethod (setf vertex-array) :after ((vao vertex-array) (emitter particle-emitter))
  (setf (mesh-index-buffer emitter) (or (index-buffer vao) (error "VAO must be indexed!")))
  (loop for binding in (bindings vao)
        do (when (and (listp binding) (= (vertex-attribute-order 'location) (getf (rest binding) :index)))
             ;; FIXME: this does not consider the :OFFSET.
             (unless (= 0 (getf (rest binding) :offset 0)) (implement!))
             (let* ((buffer (first binding))
                    (stride (floor (getf (rest binding) :stride) (gl-type-size (element-type buffer)))))
               (setf (mesh-vertex-buffer emitter) buffer)
               (setf (mesh-vertex-stride emitter) stride)
               (return)))
        finally (error "VAO must have a position binding at index 0.")))

(defmethod (setf particle-force-fields) ((cons cons) (emitter particle-emitter))
  (when (or (not (slot-boundp emitter 'particle-force-fields)))
    ;; We have a hard max of 32 anyway in the shader....
    (setf (particle-force-fields emitter) (make-instance 'particle-force-fields :size 32)))
  (let ((size (length cons))
        (struct (particle-force-fields emitter)))
    (when (< 32 size)
      (error "Cannot support more than 32 particle force fields!"))
    ;; FIXME: copy over and resize instead of this nonsense.
    (setf (slot-value struct 'particle-force-field-count) size)
    (loop for info in cons
          for i from 0
          for target = (elt (slot-value struct 'particle-force-fields) i)
          do (destructuring-bind (&key (type :point) (position (vec 0 0 0)) (strength 0.0) (range 0.0) (normal +vy3+)) info
               (setf (slot-value target 'type) (map-particle-force-field-type type))
               (setf (slot-value target 'position) position)
               (setf (slot-value target 'strength) strength)
               (setf (slot-value target 'range) range)
               (setf (slot-value target 'inv-range) (if (= 0.0 range) 0.0 (/ range)))
               (setf (slot-value target 'normal) normal)))))

(defmethod emit ((name symbol) count &rest particle-options &key &allow-other-keys)
  (apply #'emit (node name T) count particle-options))
