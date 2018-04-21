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
  (error "COLLIDES-P not defined"))

(defmethod resolve ((entity collidable-entity) (other collidable-entity) &key data preserve-impulse)
  (declare (ignore entity other data preserve-impulse))
  (error "RESOLVE not defined"))


(define-shader-entity collidable-point-entity (collidable-entity) ())

(defmethod collides-p ((entity collidable-point-entity) (other collidable-point-entity))
  (let ((loc-a (offset-location entity))
        (loc-b (offset-location other)))
    ;; let's ignore tiny sub-pixels with that 0.05
    (<= (vlength (v- loc-a loc-b)) 0.05)))

(defmethod collides-p ((entity collidable-point-entity) (other collidable-sphere-entity))
  (let ((loc-a (offset-location entity))
        (loc-b (offset-location other)))
    (<= (vlength (v- loc-a loc-b)) (range other))))

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
    (<= (vlength (v- loc-a loc-b)) (+ (range entity) (range other)))))

(defmethod collides-p ((entity collidable-sphere-entity) (other collidable-box-entity))
  (let* ((loc (offset-location entity))
         (loc-b (offset-location other))
         (diff (v- loc-b loc))
         (distance (vlength diff))
         (normal (vunit diff))
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
           (diff (v- loc-b loc))
           (distance (vlength diff))
           (normal (vunit diff))
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
  (let* ((loc (offset-location entity))
         (loc-b (offset-location other))
         (min (v- loc (/ (side entity) 2.0)))
         (max (v+ loc (/ (side entity) 2.0)))
         (min-b (v- loc-b (/ (side other) 2.0)))
         (max-b (v+ loc-b (/ (side other) 2.0))))
    (and (ranges-overlap-p (vx min) (vx max) (vx min-b) (vx max-b))
         (ranges-overlap-p (vy min) (vy max) (vy min-b) (vy max-b))
         (typecase loc
           (vec3 (ranges-overlap-p (vz min) (vz max) (vz min-b) (vz max-b)))
           (vec4
            (ranges-overlap-p (vz min) (vz max) (vz min-b) (vz max-b))
            (ranges-overlap-p (vw min) (vw max) (vw min-b) (vw max-b)))
           (T T)))))

(defmethod collides-p ((entity collidable-box-entity) (other collidable-verlet-entity))
  ;; FIXME: Turns the verlet shape into a box
  (multiple-value-bind (min-b max-b)
      (min-max-points other)
    (let* ((loc (offset-location entity))
           (loc-b (offset-location other))
           (min (v- loc (/ (side entity) 2.0)))
           (max (v+ loc (/ (side entity) 2.0))))
      (and (ranges-overlap-p (vx min) (vx max) (vx min-b) (vx max-b))
           (ranges-overlap-p (vy min) (vy max) (vy min-b) (vy max-b))
           (typecase loc
             (vec3 (ranges-overlap-p (vz min) (vz max) (vz min-b) (vz max-b)))
             (vec4
              (ranges-overlap-p (vz min) (vz max) (vz min-b) (vz max-b))
              (ranges-overlap-p (vw min) (vw max) (vw min-b) (vw max-b)))
             (T T))))))


(define-shader-entity collidable-verlet-entity (collidable-entity verlet-entity) ())

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-point-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-sphere-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-box-entity))
  (collides-p other entity))

(defmethod collides-p ((entity collidable-verlet-entity) (other collidable-verlet-entity))
  (let ((data ()))
    (loop with first-a = (first (mass-points entity))
          with first-b = (first (mass-points other))
          for points-a = (mass-points entity) then (when points-a (rest points-a))
          for points-b = (mass-points other) then (if points-a points-b (rest points-b))
          while (or points-a points-b)
          for point = (if points-a (first points-a) (first points-b))
          for parent = (if points-a entity other)
          for edge = (cons point (if points-a
                                     (or (second points-a) first-a)
                                     (or (second points-b) first-b)))
          for axis = (vunit (v- (location (car edge)) (location (cdr edge))))
          for (min-a max-a) = (multiple-value-list (project-to-axis entity axis))
          for (min-b max-b) = (multiple-value-list (project-to-axis other axis))
          for distance = (if (< min-a max-a) (- min-b max-a) (- min-a max-b))
          when (< 0.0 distance) do (setf data NIL)
          until (< 0.0 distance)
          minimizing distance into smallest-distance
          when (<= distance smallest-distance)
          do (setf data (list :normal axis
                              :edge edge
                              :entity parent
                              :other (if (eql parent entity) other entity))))
    (when data
      (let ((axis (getf data :normal))
            (loc-other (location (getf data :other))))
        (when (< 0.0 (v. (getf data :normal)
                         (v- (location (getf data :entity))
                             loc-other)))
          (nv* axis -1.0)
          (setf (getf data :normal) axis))
        (for:for ((point in (mass-points (getf data :entity)))
                  (loc = (location point))
                  (distance = (vlength (v* axis (v- loc loc-other))))
                  (smallest-d minimizing distance)
                  (nearest-point = (if (= smallest-d distance) point nearest-point)))
          (returning (append data (list :depth distance :vertex nearest-point))))))))

(defmethod resolve ((entity collidable-verlet-entity) (other collidable-box-entity)
                    &key data preserve-impulse)
  ;; FIXME: support 3D
  (when data
    (let* ((loc (location (getf data :vertex)))
           (col-vec (v* (getf data :normal) (getf data :depth)))
           (mass-a (mass (getf data :entity)))
           (mass-b (mass (getf data :other)))
           (total-mass (+ mass-a mass-b))
           (edge (getf data :edge))
           (edge-a (location (car edge)))
           (edge-b (location (cdr edge)))
           (nearest (location (getf data :vertex)))
           (t-factor (if (< (abs (- (vy edge-a) (vy edge-b)))
                            (abs (- (vx edge-a) (vx edge-b))))
                         (/ (- (vx nearest) (vx col-vec) (vx edge-a))
                            (- (vx edge-b) (vx edge-a)))
                         (/ (- (vy nearest) (vy col-vec) (vy edge-a))
                            (- (vy edge-b) (vy edge-a)))))
           (lmdb (/ 1.0 (+ (* t-factor t-factor) (* (- 1.0 t-factor) (- 1.0 t-factor))))))
      (let ((mass-a (/ mass-a total-mass))
            (mass-b (/ mass-b total-mass)))
        (nv- (location (car edge)) (v* col-vec (- 1.0 t-factor) mass-a lmdb))
        (nv- (location (cdr edge)) (v* col-vec t-factor mass-a lmdb))
        (nv+ (location (getf data :vertex)) (v* col-vec mass-b))
        (when preserve-impulse
          (let ((col-vec (v* col-vec
                             (v+ loc (location (getf data :vertex)))
                             (/ (getf data :depth)) 0.5))) ;; 0.5 for damping
            (setf (old-location (car edge)) (v- (location (car edge))
                                                (v* col-vec mass-a))
                  (old-location (cdr edge)) (v- (location (cdr edge))
                                                (v* col-vec mass-a))
                  (old-location (getf data :vertex)) (v+ (location (getf data :vertex))
                                                         (v* col-vec mass-b)))))))))

(defmethod min-max-points ((entity collidable-verlet-entity))
  (let ((min (location entity))
        (max (location entity)))
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
  (let* ((dotp (v. axis (offset-location entity)))
         (min dotp)
         (max dotp))
    (for:for ((point in (mass-points entity))
              (loc = (location point))
              (dotp = (v. axis loc))
              (min minimize dotp)
              (max maximize dotp))
      (returning (values min max)))))


(defun ranges-overlap-p (min-a max-a min-b max-b)
  (and (<= min-b max-a) (<= min-a max-b)))
