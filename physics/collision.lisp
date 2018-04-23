#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(define-shader-entity collidable-entity (physical-entity) ())
(define-shader-entity collidable-point-entity (collidable-entity) ())
(define-shader-entity collidable-sphere-entity (collidable-entity) ())
(define-shader-entity collidable-box-entity (collidable-entity) ())
(define-shader-entity collidable-verlet-entity (collidable-entity verlet-entity) ())

(defgeneric collides-p (entity other))
(defgeneric resolve (entity other &key data preserve-impulse))

(define-shader-entity collidable-entity (physical-entity)
  ((offset :initarg :offset :accessor offset)))

(defmethod initialize-instance :after ((entity collidable-entity) &key offset)
  (let ((zero-v (v* (location entity) 0.0)))
    (setf (offset entity) (if offset
                              (ensure-vector-type offset
                                                  (type-of zero-v)
                                                  zero-v)
                              zero-v))))

(defmethod offset-location ((entity collidable-entity))
  (v+ (location entity) (or (offset entity) 0)))

(defmethod collides-p ((entity collidable-entity) (other collidable-entity))
  (declare (ignore entity other))
  (values))

(defmethod resolve ((entity collidable-entity) (other collidable-entity) &key data preserve-impulse)
  (declare (ignore entity other data preserve-impulse))
  (values))


(define-shader-entity collidable-point-entity (collidable-entity) ())

(defmethod collides-p ((entity collidable-point-entity) (other collidable-point-entity))
  (let ((loc-a (offset-location entity))
        (loc-b (offset-location other)))
    ;; let's ignore tiny sub-pixels with that 0.05
    (<= (vdistance loc-a loc-b) 0.05)))

(defmethod collides-p ((entity collidable-point-entity) (other collidable-sphere-entity))
  (let ((loc-a (offset-location entity))
        (loc-b (offset-location other)))
    (<= (vdistance loc-a loc-b) (range other))))

(defmethod collides-p ((entity collidable-point-entity) (other collidable-box-entity))
  (let* ((loc (offset-location entity))
         (loc-b (offset-location other))
         (min (v- loc-b (/ (side other) 2.0)))
         (max (v+ loc-b (/ (side other) 2.0))))
    (etypecase loc
      (vec2 (and (<= (vx min) (vx loc) (vx max))
                 (<= (vy min) (vy loc) (vy max))))
      (vec3 (and (<= (vx min) (vx loc) (vx max))
                 (<= (vy min) (vy loc) (vy max))
                 (<= (vz min) (vz loc) (vz max))))
      (vec3 (and (<= (vx min) (vx loc) (vx max))
                 (<= (vy min) (vy loc) (vy max))
                 (<= (vz min) (vz loc) (vz max))
                 (<= (vw min) (vw loc) (vw max)))))))

(defmethod collides-p ((entity collidable-point-entity) (other collidable-verlet-entity))
  ;; FIXME: Turns the verlet shape into a box
  (multiple-value-bind (min max)
      (min-max-points other)
    (let* ((loc (offset-location entity)))
      (etypecase loc
        (vec2 (and (<= (vx min) (vx loc) (vx max))
                   (<= (vy min) (vy loc) (vy max))))
        (vec3 (and (<= (vx min) (vx loc) (vx max))
                   (<= (vy min) (vy loc) (vy max))
                   (<= (vz min) (vz loc) (vz max))))
        (vec3 (and (<= (vx min) (vx loc) (vx max))
                   (<= (vy min) (vy loc) (vy max))
                   (<= (vz min) (vz loc) (vz max))
                   (<= (vw min) (vw loc) (vw max))))))))


(define-shader-entity collidable-sphere-entity (collidable-entity)
  ((range :initarg :range :accessor range)))

(defmethod collides-p ((entity collidable-sphere-entity) (other collidable-point-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-sphere-entity) (other collidable-sphere-entity))
  (let ((loc-a (offset-location entity))
        (loc-b (offset-location other)))
    (<= (vdistance loc-a loc-b) (+ (range entity) (range other)))))

(defmethod collides-p ((entity collidable-sphere-entity) (other collidable-box-entity))
  (let* ((loc (offset-location entity))
         (loc-b (offset-location other))
         (distance (vdistance loc-b loc))
         (normal (vunit (v- loc-b loc)))
         (nearest (v* normal (range entity)))
         (min (v- loc-b (/ (side other) 2.0)))
         (max (v+ loc-b (/ (side other) 2.0))))
    (etypecase nearest
      (vec2 (and (<= (vx min) (vx nearest) (vx max))
                 (<= (vy min) (vy nearest) (vy max))))
      (vec3 (and (<= (vx min) (vx nearest) (vx max))
                 (<= (vy min) (vy nearest) (vy max))
                 (<= (vz min) (vz nearest) (vz max))))
      (vec3 (and (<= (vx min) (vx nearest) (vx max))
                 (<= (vy min) (vy nearest) (vy max))
                 (<= (vz min) (vz nearest) (vz max))
                 (<= (vw min) (vw nearest) (vw max)))))))

(defmethod collides-p ((entity collidable-sphere-entity) (other collidable-verlet-entity))
  ;; FIXME: Turns the verlet shape into a box
  (multiple-value-bind (min max)
      (min-max-points other)
    (let* ((loc (offset-location entity))
           (loc-b (offset-location other))
           (distance (vdistance loc-b loc))
           (normal (vunit (v- loc-b loc)))
           (nearest (v* normal (range entity))))
      (etypecase nearest
        (vec2 (and (<= (vx min) (vx nearest) (vx max))
                   (<= (vy min) (vy nearest) (vy max))))
        (vec3 (and (<= (vx min) (vx nearest) (vx max))
                   (<= (vy min) (vy nearest) (vy max))
                   (<= (vz min) (vz nearest) (vz max))))
        (vec3 (and (<= (vx min) (vx nearest) (vx max))
                   (<= (vy min) (vy nearest) (vy max))
                   (<= (vz min) (vz nearest) (vz max))
                   (<= (vw min) (vw nearest) (vw max))))))))


(define-shader-entity collidable-box-entity (collidable-entity)
  ((side :initarg :side :accessor side))
  (:default-initargs :side (error "SIDE required")))

(defmethod collides-p ((entity collidable-box-entity) (other collidable-point-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-box-entity) (other collidable-sphere-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-box-entity) (other collidable-box-entity))
  (let* ((loc-a (offset-location entity))
         (loc-b (offset-location other))
         (half-a (/ (side entity) 2.0))
         (half-b (/ (side other) 2.0)))
    (aabb-check (v- loc-a half-a) (v+ loc-a half-a))))

(defmethod collides-p ((entity collidable-box-entity) (other collidable-verlet-entity))
  ;; FIXME: Turns the verlet shape into a box
  (multiple-value-bind (min-b max-b)
      (min-max-points other)
    (let* ((loc-a (offset-location entity))
           (half-a (/ (side entity) 2.0)))
      (aabb-check (v- loc-a half-a) (v+ loc-a half-a) min-b max-b))))


(define-shader-entity collidable-verlet-entity (collidable-entity verlet-entity) ())

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-point-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-sphere-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-box-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-verlet-entity))
  (multiple-value-bind (min-a max-a)
      (min-max-points entity)
    (multiple-value-bind (min-b max-b)
        (min-max-points other)
      (when (aabb-check min-a max-a min-b max-b)
        (let ((data ()))
          (loop with first-a = (first (mass-points entity))
                with first-b = (first (mass-points other))
                for points-a = (rest (mass-points entity)) then (when points-a (rest points-a))
                for points-b = (rest (mass-points other)) then (if points-a points-b (rest points-b))
                while (or points-a points-b)
                for point = (if points-a (first points-a) (first points-b))
                for parent = (if points-a entity other)
                for edge = (cons point (or (when points-a (second points-a) first-a)
                                           (second points-b) first-b))
                for edge-start = (location (car edge))
                for edge-end = (location (cdr edge))
                for square-dist = (square-distance edge-start edge-end)
                while (< 0.0 square-dist)
                for axis = (perpendicular edge-start edge-end)
                for (min-a max-a) = (multiple-value-list (project-to-axis entity axis))
                for (min-b max-b) = (multiple-value-list (project-to-axis other axis))
                for distance = (if (< min-a min-b) (- min-b max-a) (- min-a max-b))
                when (< 0.0 distance) do (setf data NIL)
                until (< 0.0 distance)
                minimizing distance into smallest-distance
                when (<= distance smallest-distance)
                do (setf data (list :axis axis
                                    :edge edge
                                    :entity parent
                                    :other (if (eql parent entity) other entity))))
          (when data
            (let ((axis (getf data :axis))
                  (loc-entity (location (getf data :entity)))
                  (loc-other (location (getf data :other)))
                  (smallest-dist +positive-infinity+)
                  (ret-point NIL))
              (when (< (v. (v- loc-entity loc-other) axis)
                       0.0)
                (nv* axis -1.0))
              (for:for ((point in (mass-points (getf data :entity)))
                        (loc = (location point))
                        (distance = (v. axis (v- loc loc-other))))
                (when (< distance smallest-dist)
                  (setf smallest-dist distance
                        ret-point point)))
              (append data (list :depth smallest-dist :vertex ret-point)))))))))

(defvar *dudu* NIL)
(progn
  (setf *dudu* T))
(defmethod resolve ((entity collidable-verlet-entity) (other collidable-verlet-entity)
                    &key data preserve-impulse)
  ;; FIXME: support 3D
  (when data
    (when *dudu*
      (v:warn :dudu "~%~a~%" data)
      (setf *dudu* NIL))
    (let* ((edge (getf data :edge))
           (edge-start (location (car edge)))
           (edge-end (location (cdr edge)))
           (vertex (getf data :vertex))
           (vertex-loc (location vertex))
           (axis (getf data :axis))
           (depth (getf data :depth))
           (response (v* axis depth))
           (entity (getf data :entity))
           (other (getf data :other))
           (mass-entity (mass entity))
           (mass-other (mass other))
           (total-mass (+ mass-entity mass-other))
           (t-factor (if (< (abs (- (vy edge-start) (vy edge-end)))
                            (abs (- (vx edge-start) (vx edge-end))))
                         (/ (- (vx vertex-loc) (vx response) (vx edge-start))
                            (- (vx edge-end) (vx edge-start)))
                         (/ (- (vy vertex-loc) (vy response) (vy edge-start))
                            (- (vy edge-end) (vy edge-start)))))
           (t-minus (- 1.0 t-factor))
           (lmdb (/ 1.0 (+ (* t-factor t-factor) (* t-minus t-minus)))))
      (let* ((mass-entity (/ mass-entity total-mass))
             (mass-other (/ mass-other total-mass))
             (response-entity (ensure-vector-type (v* response mass-other lmdb)
                                                  (type-of vertex-loc)))
             (response-other (ensure-vector-type (v* response mass-entity)
                                                 (type-of vertex-loc))))
        (nv- (location (car edge)) (v* response-entity t-minus))
        (nv- (location (cdr edge)) (v* response-entity t-factor))
        (nv+ (location vertex) response-other)
        (when preserve-impulse
          (nv* response-entity 0.98) ;; 0.98 for damping
          (nv* response-other 0.98)
          (nv- (old-location (car edge)) (v* response-entity t-minus))
          (nv- (old-location (cdr edge)) (v* response-entity t-factor))
          (nv+ (old-location vertex) response-other))))))

(defmethod min-max-points ((entity collidable-verlet-entity))
  (let ((min (offset-location entity))
        (max (offset-location entity)))
    (for:for ((point in (mass-points entity))
              (loc = (location point)))
      ;; min
      (when (< (vx loc) (vx min))
        (setf (vx min) (vx loc)))
      (when (< (vy loc) (vy min))
        (setf (vy min) (vy loc)))
      (when (and (or (typep loc 'vec3)
                     (typep loc 'vec4))
                 (< (vz loc) (vz min)))
        (setf (vz min) (vz loc))
        (when (and (typep loc 'vec4)
                   (< (vz loc) (vz min)))
          (setf (vz min) (vz loc))))
      ;; max
      (when (< (vx max) (vx loc))
        (setf (vx max) (vx loc)))
      (when (< (vy max) (vy loc))
        (setf (vy max) (vy loc)))
      (when (and (or (typep loc 'vec3)
                     (typep loc 'vec4))
                 (< (vz max) (vz loc)))
        (setf (vz max) (vz loc))
        (when (and (typep loc 'vec4)
                   (< (vz max) (vz loc)))
          (setf (vz max) (vz loc)))))
    (values min max)))

(defmethod project-to-axis ((entity collidable-verlet-entity) axis)
  ;; TODO: more than 2D?
  (let ((axis (vec2 (vx axis) (vy axis))))
    (for:for ((point in (mass-points entity))
              (loc = (location point))
              (dotp = (v. axis (vec2 (vx loc) (vy loc))))
              (min minimize dotp)
              (max maximize dotp))
      (returning (values min max)))))


(defun aabb-check (min-a max-a min-b max-b)
  (and (ranges-overlap-p (vx min-a) (vx max-a) (vx min-b) (vx max-b))
       (ranges-overlap-p (vy min-a) (vy max-a) (vy min-b) (vy max-b))
       (typecase min-a
         (vec3 (ranges-overlap-p (vz min-a) (vz max-a) (vz min-b) (vz max-b)))
         (vec4
          (ranges-overlap-p (vz min-a) (vz max-a) (vz min-b) (vz max-b))
          (ranges-overlap-p (vw min-a) (vw max-a) (vw min-b) (vw max-b)))
         (T T))))

(defun ranges-overlap-p (min-a max-a min-b max-b)
  (and (<= min-b max-a) (<= min-a max-b)))
