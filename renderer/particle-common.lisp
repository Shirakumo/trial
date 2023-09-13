(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity particle-emitter (standalone-shader-entity transformed-entity renderable listener)
  ((texture :initform (// 'trial 'missing) :initarg :texture :accessor texture)
   (to-emit :initform 0.0 :initarg :to-emit :accessor to-emit)
   (particle-rate :initform 0.0 :initarg :particle-rate :accessor particle-rate)
   (vertex-array :initform (// 'trial 'unit-square) :accessor vertex-array)
   (max-particles :initarg :max-particles :initform 1000 :reader max-particles)
   (motion-blur :initarg :motion-blur :initform 0.0 :uniform T))
  (:buffers (trial standard-environment-information)))

(defmethod shared-initialize :after ((emitter particle-emitter) slots &key particle-options particle-force-fields vertex-array)
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
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (if (logbitp 29 int) :quad :billboard)))

(defmethod (setf particle-mode) (mode (emitter particle-emitter))
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (setf (ldb (byte 1 29) int) (ecase mode
                                  (:quad 1)
                                  (:billboard 0)))
    (setf (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer))) int)
    mode))

(defmethod particle-flip ((emitter particle-emitter))
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (case (ldb (byte 2 30) int)
      (0 NIL)
      (1 :y)
      (2 :x)
      (3 T))))

(defmethod (setf particle-flip) (flip (emitter particle-emitter))
  (let ((int (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer)))))
    (setf (ldb (byte 2 30) int) (ecase flip
                                  ((NIL) 0)
                                  (:y 1)
                                  (:x 2)
                                  ((T) 3)))
    (setf (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer))) int)
    flip))

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
    (setf (particle-color (buffer-data (slot-value emitter 'particle-emitter-buffer))) int)
    color))

(defmethod (setf vertex-array) ((resource placeholder-resource) (emitter particle-emitter))
  (setf (vertex-array emitter) (ensure-generated resource)))

(defmethod (setf vertex-array) :after ((vao vertex-array) (emitter particle-emitter))
  (setf (mesh-index-buffer emitter) (or (index-buffer vao) (error "VAO must be indexed!")))
  (loop for binding in (bindings vao)
        do (when (and (listp binding) (= 0 (getf (rest binding) :index)))
             (let ((stride (floor (getf (rest binding) :stride) (gl-type-size (element-type (first binding)))))
                   (buffer (first binding)))
               (setf (mesh-vertex-buffer emitter) buffer)
               (setf (mesh-vertex-stride emitter) stride)))
        finally (error "VAO must have a position binding at index 0.")))
