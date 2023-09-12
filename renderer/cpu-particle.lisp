(in-package #:org.shirakumo.fraf.trial)

(defstruct (raw-particle
            (:constructor make-raw-particle (&optional location velocity))
            (:copier NIL)
            (:predicate NIL))
  (location (vec 0 0 0) :type vec3)
  (velocity (vec 0 0 0) :type vec3))

(defun %simulate-particle (data in out dt force-fields)
  (declare (type (simple-array single-float (*)) data))
  (declare (type (unsigned-byte 32) in out))
  (declare (type single-float dt))
  (let* ((loc (vec (aref data (+ in 0))
                   (aref data (+ in 1))
                   (aref data (+ in 2))))
         (vel (vec (aref data (+ in 3))
                   (aref data (+ in 4))
                   (aref data (+ in 5))))
         (acc (vec 0 0 0))
         (tt (aref data (+ in 6)))
         (particle (make-raw-particle loc vel)))
    (declare (dynamic-extent loc vel acc particle))
    ;; Apply force fields
    (loop for field across force-fields
          do (apply-force field particle dt))
    ;; Simulate
    (nv+* vel acc dt)
    (nv+* loc vel dt)
    ;; Store
    (setf (aref data (+ out 0)) (vx loc))
    (setf (aref data (+ out 1)) (vy loc))
    (setf (aref data (+ out 2)) (vz loc))
    (setf (aref data (+ out 3)) (vx vel))
    (setf (aref data (+ out 4)) (vy vel))
    (setf (aref data (+ out 5)) (vz vel))
    (setf (aref data (+ out 6)) (- tt dt))))

(defun %simulate-particles (particles live-particles free-list dt force-fields)
  ;; Returns updated live particle count
  (loop with out = 0
        for in from 0 below (* live-particles 8) by 8
        for life = (%simulate-particle particles in out dt force-fields)
        do (if (<= life 0)
               (vector-push (aref particles (+ in 7)) free-list)
               (incf out 8))
        finally (return (truncate out 8))))

(defun random-point-on-mesh-surface (vertex-data vertex-stride faces randoms &optional (location (vec3)) (normal (vec3)))
  (declare (type (simple-array single-float (*)) vertex-data))
  (declare (type (simple-array (unsigned-byte 32) (*)) faces))
  (declare (type (unsigned-byte 32) vertex-stride))
  (declare (type vec3 location normal randoms))
  (declare (optimize speed))
  (cond ((< (length vertex-data) (* 3 vertex-stride))
         ;; Special handling for point meshes
         (setf (vx location) (aref vertex-data 0))
         (setf (vy location) (aref vertex-data 1))
         (setf (vz location) (aref vertex-data 2))
         (setf (vx normal) (aref vertex-data 3))
         (setf (vy normal) (aref vertex-data 4))
         (setf (vz normal) (aref vertex-data 5)))
        (T
         ;; Pick a random triangle and read out the properties
         (let* ((tri (truncate (* (length faces) (vz randoms))))
                (i0 (* vertex-stride (aref faces (+ tri 0))))
                (i1 (* vertex-stride (aref faces (+ tri 1))))
                (i2 (* vertex-stride (aref faces (+ tri 2))))
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
             (values location (nvunit normal)))))))

(defun %emit-particle (particles properties pos prop randoms randomness
                      matrix velocity rotation lifespan size scaling color
                      vertex-data vertex-stride faces)
  (declare (type (simple-array single-float (*)) particles properties vertex-data))
  (declare (type (simple-array (unsigned-byte 32) (*)) faces))
  (declare (type (unsigned-byte 32) pos prop))
  (declare (type single-float lifespan randomness size scaling rotation))
  (declare (type vec3 velocity randoms))
  (let* ((location (vec 0 0 0))
         (normal (vec 0 0 0))
         (velocity (vcopy velocity)))
    (declare (dynamic-extent location normal velocity))
    ;; Evaluate static particle properties first
    (random-point-on-mesh-surface vertex-data vertex-stride faces (vz randoms) location normal)
    (n*m4/3 matrix normal)
    (nv* velocity (nv+ (v* (v- randoms 0.5) randomness) normal))
    (setf (aref properties (+ prop 0)) (+ lifespan (* lifespan randomness (- (vx randoms) 0.5))))
    (setf (aref properties (+ prop 1)) (* rotation randomness (- (vz randoms) 0.5)))
    (setf (aref properties (+ prop 2)) (+ size (* size randomness (- (vy randoms) 0.5))))
    (setf (aref properties (+ prop 3)) (* (aref properties (+ prop 2)) scaling))
    ;; Randomise flip if activated
    (when (logbitp 30 color) (setf (ldb (byte 1 30) color) (round (vx randoms))))
    (when (logbitp 31 color) (setf (ldb (byte 1 31) color) (round (vy randoms))))
    (setf (aref properties (+ prop 4)) (float-features:bits-single-float color))
    (replace properties (marr4 matrix) :start1 (+ prop 5))
    ;; Now set dynamic particle properties
    (setf (aref particles (+ pos 0)) (vx location))
    (setf (aref particles (+ pos 1)) (vy location))
    (setf (aref particles (+ pos 2)) (vz location))
    (setf (aref particles (+ pos 3)) (vx velocity))
    (setf (aref particles (+ pos 4)) (vy velocity))
    (setf (aref particles (+ pos 5)) (vz velocity))
    (setf (aref particles (+ pos 6)) (aref properties (+ prop 0)))
    (setf (aref particles (+ pos 7)) (float prop))))

(defun %allocate-particle-data (max-particles)
  (let ((particles (make-array (* max-particles 8) :element-type 'single-float))
        (properties (make-array (* max-particles 21) :element-type 'single-float))
        (free-list (make-array max-particles :element-type '(unsigned-byte 32) :fill-pointer 0)))
    (dotimes (i max-particles (values particles properties free-list))
      (vector-push (* i 21) free-list))))

(define-shader-entity cpu-particle-emitter (standalone-shader-entity transformed-entity renderable listener)
  (particles
   properties
   free-list
   particle-buffer
   particle-property-buffer
   (live-particles :initform 0 :accessor live-particles)
   (max-particles :initform 32 :accessor max-particles))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "particle/cpu-render.glsl")))

(defmethod initialize-instance :after ((emitter cpu-particle-emitter) &key)
  (multiple-value-bind (particles properties free-list) (%allocate-particle-data max-particles)
    ))

(defmethod simulate-particles ((emitter cpu-particle-emitter))
  (multiple-value-bind (to-emit emit-carry) (floor (incf (to-emit particle-emitter) (* dt (particle-rate particle-emitter))))
    (dotimes (i (min to-emit (length free-list)))
      (let ((prop (vector-pop free-list))
            (mat (tmat4 (tf emitter))))
        (%emit-particle particles properties live prop (vrand (vec 0.5 0.5 0.5)) randomness
                        mat velocity rotation lifespan size scaling color
                        vertex-data vertex-stride index-data)
        (incf live-particles)))
    (setf live-particles (%simulate-particles particles live-particles free-list dt force-fields))))

(defmethod emit ((emitter cpu-particle-emitter) count &rest particle-options &key vertex-array location orientation scaling transform)
  (setf (particle-options emitter) (remf* particle-options :vertex-array :location :orientation :scaling :transform))
  (when transform (setf (tf emitter) transform))
  (when location (setf (location emitter) location))
  (when scaling (setf (scaling emitter) scaling))
  (when orientation (setf (orientation emitter) orientation))
  (when vertex-array (setf (vertex-array emitter) vertex-array))
  (with-all-slots-bound (emitter cpu-particle-emitter)
    (dotimes (i (min count (length free-list)))
      (let ((prop (vector-pop free-list))
            (mat (tmat4 (tf emitter))))
        (%emit-particle particles properties live prop (vrand (vec 0.5 0.5 0.5)) randomness
                        mat velocity rotation lifespan size scaling color
                        vertex-data vertex-stride index-data)
        (incf live-particles)))))

(defmethod clear ((emitter cpu-particle-emitter))
  (setf (live-particles emitter) 0)
  (let ((free-list (free-list emitter)))
    (setf (fill-pointer free-list) 0)
    (dotimes (i (max-particles emitter))
      (vector-push (* i 21) free-list))))

(defmethod bind-textures ((emitter cpu-particle-emitter))
  (gl:active-texture :texture0)
  (gl:bind-texture (target (texture emitter)) (gl-name (texture emitter))))

(defmethod render :before ((emitter cpu-particle-emitter) (program shader-program))
  (gl:depth-mask NIL)
  (gl:bind-vertex-array (gl-name (// 'trial 'empty-vertex-array))))

(defmethod render :after ((emitter cpu-particle-emitter) (program shader-program))
  (gl:bind-vertex-array 0)
  (gl:depth-mask T))

(defmethod render ((emitter cpu-particle-emitter) (program shader-program))
  (gl:blend-func :src-alpha :one)
  (%gl:draw-arrays-instanced :triangles 0 6 (live-particles emitter))
  (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha))
