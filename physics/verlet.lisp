#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.
In general, 4 is minimum for an alright accuracy, 8 is enough for a good accuracy.")


(defclass verlet-point (located-entity) ())
(define-shader-entity verlet-entity (physical-entity vertex-entity) ())


(defclass verlet-point (located-entity)
  ((old-location :initform NIL :accessor old-location)
   (acceleration :initform NIL :accessor acceleration)
   (viscosity :initarg :viscosity :accessor viscosity)
   (mass :initarg :mass :accessor mass))
  (:default-initargs :mass 1.0
                     :viscosity 1.0))

(defmethod initialize-instance :after ((point verlet-point) &key old-location)
  (setf (old-location point) (or old-location (location point))))

(defmethod apply-force ((point verlet-point) force)
  (when force
    (if (vec-p (acceleration point))
        (nv+ (acceleration point) force)
        (setf (acceleration point) (v+ (v* (location point) 0.0) force)))))

(defmethod inertia ((point verlet-point))
  (let ((move (vcopy (location point)))
        (viscosity (viscosity point))
        (old (old-location point)))
    (setf (old-location point) (vcopy move))
    (nv* move viscosity)
    (nv* old viscosity)
    (when (acceleration point)
      (nv+ move (acceleration point)))
    (nv+ (location point) move)
    (nv- (location point) old)
    (if (acceleration point)
        (nv* (acceleration point) 0.0)
        (setf (acceleration point) (nv* move 0.0)))))

(define-shader-entity verlet-entity (physical-entity vertex-entity)
  ((mass-points :initform NIL :accessor mass-points)
   (constraints :initform NIL :accessor constraints)
   (center :initform NIL :accessor center)
   (viscosity :initarg :viscosity :reader viscosity))
  (:default-initargs :viscosity 1.0))

(defmethod initialize-instance :after ((entity verlet-entity)
                                       &key mass-points vertex-array location (viscosity 1.0))
  (let* ((point-array (or mass-points vertex-array))
         (mass-points (etypecase point-array
                        (mesh (vertices (input vertex-array)))
                        (vertex-mesh (mass-points point-array))
                        ((or list array) point-array))))
    (unless (< 3 (length mass-points))
      (error "MASS-POINTS or VERTEX-ARRAY required"))
    (let ((mass-points (quick-hull mass-points)))
      (setf (mass-points entity) (mapcar #'(lambda (loc)
                                             (make-instance 'verlet-point
                                                            :location (v+ location loc)
                                                            :viscosity viscosity))
                                         mass-points))
      (setf (constraints entity) (loop with first = (first (mass-points entity))
                                       for points = (mass-points entity) then (rest points)
                                       while points
                                       for current = (first points)
                                       for next = (or (second points) first)
                                       collect (make-instance 'distance-constraint :point-a current
                                                                                   :point-b next))))
    (let ((center (make-instance 'verlet-point :location location)))
      (push (make-instance 'distance-constraint :point-a center
                                                :point-b (first (mass-points entity)))
            (constraints entity))
      (push (make-instance 'distance-constraint :point-a center
                                                :point-b (third (mass-points entity)))
            (constraints entity))
      (push (make-instance 'distance-constraint :point-a center
                                                :point-b (third (mass-points entity)))
            (constraints entity))
      (push center (mass-points entity))
      (setf (center entity) center))))

(defmethod location :around ((entity verlet-entity))
  (let ((center (location (center entity))))
    (unless (v= center (call-next-method))
      (setf (slot-value entity 'location) center))
    center))

(defmethod (setf location) (value (entity verlet-entity))
  (declare (type vec value))
  (let ((diff (v- value (location (center entity)))))
    (for:for ((point in (mass-points entity)))
      (nv+ (location point) diff)))
  (call-next-method))

(defmethod (setf viscosity) (value (entity verlet-entity))
  (declare (type number value))
  (setf (slot-value entity 'viscosity) value)
  (for:for ((point in (mass-points entity)))
    (setf (viscosity point) value)))

(defmethod apply-force ((entity verlet-entity) force)
  (let ((static (static-forces entity)))
    (when (or force static)
      (let ((force (if (and static force)
                       (nv+ force static)
                       (if force force static))))
        (for:for ((point in (mass-points entity)))
          (apply-force point force))))))

(defmethod inertia ((entity verlet-entity))
  (for:for ((point in (mass-points entity)))
    (inertia point)))

(defmethod constrain-to-frame ((entity verlet-entity) min max)
  (let ((center (location (center entity))))
    (for:for ((point in (mass-points entity))
              (diff = (v- (location point) center)))
      (push (make-instance 'frame-constraint :min (v+ min diff)
                                             :max (v+ max diff)
                                             :point point)
            (constraints entity)))))


(defun verlet-simulation (entities &key forces (iterations *iterations*))
  ;; Simulations
  (let ((collidables ()))
    (for:for ((entity in entities))
      (when (typep entity 'collidable-entity)
        (push entity collidables)))
    (dotimes (i iterations)
      ;; Check constraints
      (for:for ((entity in entities))
        (for:for ((constraint in (constraints entity)))
          (relax constraint)))
      ;; Collision checks without preserving impulse
      (when collidables
        (loop for entity-list = collidables then (rest entity-list)
              while (rest entity-list)
              for entity = (first entity-list)
              do (for:for ((other in (rest entity-list))
                           (data = (collides-p entity other)))
                   (when data (resolve entity other :data data)))))
      ;; Handle inertia based on old location
      (for:for ((entity in entities))
        (apply-force entity forces)
        (inertia entity))
      ;; Check constraints and preserve impulse
      (for:for ((entity in entities))
        (for:for ((constraint in (constraints entity)))
          (relax constraint T)))
      ;; Collision checks and preserve impulse
      (when collidables
        (loop for entity-list = collidables then (rest entity-list)
              while (rest entity-list)
              for entity = (first entity-list)
              do (for:for ((other in (rest entity-list))
                           (data = (collides-p entity other)))
                   (when data
                     (resolve entity other :data data
                                           :preserve-impulse T))))))))
