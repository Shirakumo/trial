(in-package #:org.shirakumo.fraf.trial)

(defclass particle (located-entity physics-entity)
  ((acceleration :initform (vec 0 0 0) :reader acceleration)))

(defmethod (setf acceleration) ((val vec3) (particle particle))
  (v<- (acceleration particle) val))

(defmethod integrate ((particle particle) dt)
  (nv+* (location particle) (velocity particle) dt)
  (nv+* (location particle) (acceleration particle) (* 0.5 dt dt))
  (nv* (force particle) (inverse-mass particle))
  (nv+ (force particle) (acceleration particle))
  (nv+* (velocity particle) (force particle) dt)
  (nv* (velocity particle) (expt (damping particle) dt))
  (vsetf (force particle) 0 0 0))

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

(defclass hit-generator () ())

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

(defclass particle-link (hit-generator)
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

(defclass mass-aggregate-system (physics-system)
  ((hit-generators :initform (make-array 0 :adjustable T :fill-pointer T) :accessor hit-generators)))

(defmethod generate-hits ((system mass-aggregate-system) hits start end)
  (loop for generator across (hit-generators system)
        do (setf start (generate-hits generator hits start end)))
  start)

(defmethod resolve-hits ((system mass-aggregate-system) hits start end dt &key (iterations (* 2 end)))
  (loop for iterations from 0 below iterations
        for max-velocity = 0.0
        for candidate = NIL
        do (loop for i from start below end
                 for hit = (aref hits i)
                 for sep = (separating-velocity (hit-a hit) (hit-b hit) hit)
                 do (when (< sep max-velocity)
                      (setf max-velocity sep)
                      (setf candidate hit)))
           (if candidate
               (resolve-hit (hit-a candidate) (hit-b candidate) candidate dt)
               (return iterations))
        finally (return iterations)))
