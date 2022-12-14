(in-package #:org.shirakumo.fraf.trial)

(defgeneric integrate (object dt))
(defgeneric mass (object))
(defgeneric (setf mass) (object))

(defun nv+* (target vector scalar)
  (setf (vx target) (* (vx vector) scalar))
  (setf (vy target) (* (vy vector) scalar))
  (setf (vz target) (* (vz vector) scalar)))

(defclass physics-entity ()
  ((velocity :initform (vec 0 0 0) :reader velocity)
   (inverse-mass :initform 0.0 :accessor inverse-mass)))

(defmethod shared-initialize :after ((entity physics-entity) slots &key mass)
  (when mass (setf (mass entity) mass)))

(defmethod (setf velocity) ((vel vec3) (entity physics-entity))
  (v<- (velocity entity) vel))

(defmethod mass ((entity physics-entity))
  (let ((inverse (inverse-mass entity)))
    (if (= 0.0 inverse)
        0.0
        (/ (inverse-mass entity)))))

(defmethod (setf mass) (mass (entity physics-entity))
  (setf (inverse-mass entity) (if (= 0.0 mass) 0.0 (abs (float (/ mass) 0.0)))))

(defclass particle-force ()
  ())

(defgeneric apply-force (force particle dt))

(defclass particle (located-entity physics-entity)
  ((acceleration :initform (vec 0 0 0) :reader acceleration)
   (damping :initform 1.0 :accessor damping)
   (force :initform (vec 0 0 0) :reader force)))

(defmethod (setf acceleration) ((val vec3) (particle particle))
  (v<- (acceleration particle) val))

(defmethod (setf force) ((val vec3) (particle particle))
  (v<- (force particle) val))

(defmethod integrate ((particle particle) dt)
  (nv+* (location particle) (velocity particle) dt)
  (nv* (force particle) (inverse-mass particle))
  (nv+ (force particle) (acceleration particle))
  (nv+* (velocity particle) (force particle) dt)
  (vsetf (force particle) 0 0 0)
  (nv* (velocity particle) (expt (damping particle) dt)))

(defclass drag-force (particle-force)
  ((k1 :initarg :k1 :initform 1.0 :accessor k1)
   (k2 :initarg :k2 :initform 1.0 :accessor k2)))

(defmethod apply-force ((force drag-force) (particle particle) dt)
  (let* ((force (vcopy (velocity particle)))
         (coeff (vlength force))
         (coeff (+ (* (k1 force) coeff) (* (k2 force) coeff coeff))))
    (nvunit force)
    (nv* force (- coeff))
    (nv+ (force particle) force)))

(defclass spring-force (particle-force)
  ((anchor :initarg :anchor :accessor anchor)
   (spring-constant :initarg :spring-constant :initform 1.0 :accessor spring-constant)
   (rest-length :initarg :rest-length :initform 0.0 :accessor rest-length)))

(defmethod apply-force ((force spring-force) (particle particle) dt)
  (let* ((force (v- (location particle) (location (anchor force))))
         (coeff (* (abs (- (vlength force) (rest-length force)))
                   (spring-constant force))))
    (nv+ (force particle) (nv* (nvunit force) (- coeff)))))

(defclass stiff-spring-force (particle-force)
  ((anchor :initarg :anchor :accessor anchor)
   (spring-constant :initarg :spring-constant :initform 1.0 :accessor spring-constant)
   (damping :initarg :damping :initform 1.0 :accessor damping)))

(defmethod apply-force ((force stiff-spring-force) (particle particle) dt)
  (when (< 0.0 (inverse-mass particle))
    (let* ((damping (damping force))
           (relative (v- (location particle) (location (anchor force))))
           (gamma (* 0.5 (sqrt (- (* 4 (spring-constant force)) (* damping damping)))))
           (c (nv+ (v* relative (/ damping (* 2 gamma)))
                   (v* (velocity particle) (/ gamma))))
           (target (nv* (nv+ (v* relative (cos (* gamma dt)))
                             (v* c (sin (* gamma dt))))
                        (exp (* -0.5 damping dt))))
           (accel (nv- (nv* (v- target relative) (/ (* dt dt)))
                       (v* (velocity particle) dt))))
      (nv+ (force particle) (nv* accel (mass particle))))))

(defclass bungee-force (spring-force)
  ())

(defmethod apply-force ((force bungee-force) (particle particle) dt)
  (let* ((force (v- (location particle) (location (anchor force))))
         (coeff (* (- (vlength force) (rest-length force))
                   (spring-constant force))))
    (when (<= 0.0 coeff)
      (nv+ (force particle) (nv* (nvunit force) (- coeff))))))

(defstruct hit
  (a NIL :type T)
  (b NIL :type T)
  (restitution 0.0 :type single-float)
  (normal (vec 0 0 0) :type vec3)
  (depth 0.0 :type single-float))

(defmethod separating-velocity ((a particle) (b particle) hit)
  (v. (v- (velocity a) (velocity b)) (hit-normal hit)))

(defmethod separating-velocity ((a particle) (b null) hit)
  (v. (velocity a) (hit-normal hit)))

(defmethod resolve-velocity ((a particle) (b particle) hit dt)
  (let ((sep (separating-velocity a b hit))
        (total-mass (+ (inverse-mass a) (inverse-mass b))))
    (when (and (< 0 total-mass) (< sep 0))
      (let* ((new (* (- sep) (hit-restitution hit)))
             (acc-vel (v- (acceleration a) (acceleration b)))
             (acc-sep (* (v. acc-vel (hit-normal hit)) dt)))
        (when (< acc-sep 0)
          (incf new (* acc-sep (hit-restitution hit)))
          (setf new (max 0 new)))
        (let* ((delta (- new sep))
               (impulse (v* (hit-normal hit) (/ delta total-mass))))
          (nv+* (velocity a) impulse (inverse-mass a))
          (nv+* (velocity b) impulse (- (inverse-mass b))))))))

(defmethod resolve-intersection ((a particle) (b particle) hit)
  (when (<= (hit-depth hit) 0)
    (let ((total-mass (+ (inverse-mass a) (inverse-mass b))))
      (when (< 0 total-mass)
        (let ((move (v* (hit-normal hit) (/ (hit-depth hit) -1 total-mass))))
          (nv+* (location a) move (inverse-mass a))
          (nv+* (location b) move (inverse-mass b)))))))

(defmethod resolve-hit ((a particle) (b particle) hit dt)
  (resolve-velocity a b hit dt)
  (resolve-intersection a b hit))

(defun resolve-hits (hits end dt &key (max-iterations (* 2 end)))
  (loop for iterations from 0 below max-iterations
        for max-velocity = 0.0
        for candidate = NIL
        do (loop for i from 0 below end
                 for hit = (aref hits i)
                 for sep = (separating-velocity (hit-a hit) (hit-b hit) hit)
                 do (when (< sep max-velocity)
                      (setf max-velocity sep)
                      (setf candidate hit)))
           (if candidate
               (resolve-hit (hit-a candidate) (hit-b candidate) candidate dt)
               (return iterations))
        finally (return max-iterations)))

(defgeneric generate-hits (generator hits start end))

(defmacro define-hit-generation (generator &body body)
  `(defmethod generate-hits (,(enlist generator generator) hits start end)
     (let ((hit (aref hits start)))
       (block NIL
         (flet ((finish-hit ()
                  (incf start)
                  (if (< start end)
                      (setf hit (aref hits start))
                      (return))))
           ,@body))
       start)))

(defclass particle-link ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b)))

(defclass particle-cable (particle-link)
  ((max-length :initarg :max-length :accessor max-length)
   (restitution :initarg :restitution :accessor restitution)))

(define-hit-generation (link particle-cable)
  (let ((length (vdistance (location (a link)) (location (b link)))))
    (when (< (max-length link) length)
      (setf (hit-a hit) (a link))
      (setf (hit-b hit) (b link))
      (v<- (hit-normal hit) (location (b link)))
      (nv- (hit-normal hit) (location (a link)))
      (nvunit (hit-normal hit))
      (setf (hit-depth hit) (- length (max-length link)))
      (setf (hit-restitution hit) (restitution link))
      (finish-hit))))

(defclass particle-rod (particle-link)
  ((distance :initarg :distance :accessor distance)))

(define-hit-generation (link particle-rod)
  (let ((length (vdistance (location (a link)) (location (b link)))))
    (unless (= length (distance link))
      (setf (hit-a hit) (a link))
      (setf (hit-b hit) (b link))
      (v<- (hit-normal hit) (location (b link)))
      (nv- (hit-normal hit) (location (a link)))
      (nvunit (hit-normal hit))
      (cond ((< (distance link) length)
             (setf (hit-depth hit) (- length (distance link))))
            (T
             (nv- (hit-normal hit))
             (setf (hit-depth hit) (- (distance link) length))))
      (setf (hit-restitution hit) 0.0)
      (finish-hit))))

(defclass mass-aggregate-system ()
  ((particles :initform (make-array 0 :adjustable T :fill-pointer T) :accessor particles)
   (forces :initform (make-array 0 :adjustable T :fill-pointer T) :accessor forces)
   (hit-generators :initform (make-array 0 :adjustable T :fill-pointer T) :accessor hit-generators)
   (hits :initform (map-into (make-array 128) #'make-hit) :accessor hits)))

(defmethod integrate ((system mass-aggregate-system) dt)
  (loop for particle across (particles system)
        do (integrate particle dt)))

(defmethod generate-hits ((system mass-aggregate-system) hits start end)
  (loop for generator across (hit-generators system)
        do (setf start (generate-hits generator hits start end)))
  start)

(defmethod update ((system mass-aggregate-system) tt dt fc)
  (loop for particle across (particles system)
        do (loop for force across (forces system)
                 do (apply-force force particle dt)))
  (integrate system dt)
  (let* ((hits (hits system))
         (end (generate-hits system hits 0 (length hits))))
    (resolve-hits hits end dt)))
