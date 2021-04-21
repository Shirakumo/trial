#|
This file is a part of trial
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defparameter +verlet-default-gravity+ (vec3 0 -0.91 0))
(defparameter +verlet-default-mass+ 1)
(defparameter +verlet-default-friction+ 0.91)
(defparameter +verlet-default-ground-friction+ 0.66)
(defparameter +verlet-default-radius+ 5)

(define-asset (physics dot) mesh
  (make-sphere +mesh-size+))

(define-shader-entity verlet (vertex-entity colored-entity scaled-entity located-entity)
  ((old-location :initform (vec3 0 0 0) :reader old-location)
   (radius :initform NIL :accessor radius)
   (constraint-start :initform NIL :accessor constraint-start)
   (constraint-size :initform NIL :accessor constraint-size)
   (gravity :initarg :gravity :accessor gravity)
   (mass :initarg :mass :accessor mass)
   (friction :initarg :friction :accessor friction)
   (ground-friction :initarg :ground-friction :accessor ground-friction)
   (pinned :initarg :pinned :reader pinned-p :writer pinned))
  (:default-initargs :color (vec4 1 0 0 1)
                     :vertex-array (// 'physics 'dot)
                     :gravity +verlet-default-gravity+
                     :mass +verlet-default-mass+
                     :friction +verlet-default-friction+
                     :ground-friction +verlet-default-ground-friction+
                     :pinned NIL))

(defmethod initialize-instance :after ((verlet verlet) &key (radius +verlet-default-radius+)
                                                            (velocity (vec3 0 0 0))
                                                            constraint)
  (setf (radius verlet) radius)
  (setf (velocity verlet) velocity)
  (when constraint (setf (constraint verlet) constraint)))

(defmethod (setf old-location) (value (verlet verlet))
  (declare (type vec3 value))
  (let ((old (old-location verlet)))
    (setf (vx old) (vx value))
    (setf (vy old) (vy value))
    (setf (vz old) (vz value))))

(defmethod (setf radius) :after (value (verlet verlet))
  (let ((radius (radius verlet)))
    (setf (scaling verlet) (v/ (vec3 radius radius 1) +mesh-size+))))

(defmethod velocity ((verlet verlet))
  (v- (location verlet) (old-location verlet)))

(defmethod (setf velocity) (value (verlet verlet))
  (setf (old-location verlet) (v- (location verlet) value)))

(defmethod constraint ((verlet verlet))
  (let ((start (constraint-start verlet))
        (size (constraint-size verlet)))
    (when (and start size) (list start size))))

(defmethod (setf constraint) (value (verlet verlet))
  (declare (type list value))
  (let ((start (first value))
        (size (second value)))
    (declare (type (or null vec3) start size))
    (setf (constraint-start verlet) (when size start))
    (setf (constraint-size verlet) (when start size))))

(defmethod constrain ((verlet verlet))
  (let ((pos (location verlet)))
    (multiple-value-bind (constrained left right bottom top)
        (constrained-p verlet)
      (unless constrained
        (setf (vx pos) (clamp left (vx pos) right))
        (setf (vy pos) (clamp bottom (vy pos) top))))))

(defmethod constrained-p ((verlet verlet))
  (let ((pos (location verlet))
        (rad (radius verlet))
        (start (constraint-start verlet))
        (size (constraint-size verlet)))
    (unless (and start size) (return-from constrained-p T))
    (let ((left (+ (vx start) rad))
          (right (- (+ (vx start) (vx size)) rad))
          (bottom (+ (vy start) rad))
          (top (- (+ (vy start) (vy size)) rad)))
      (values (not (or (< (vx pos) left)
                       (< right (vx pos))
                       (< (vy pos) bottom)
                       (< top (vy pos))))
              left right bottom top))))

(defmethod simulate ((verlet verlet) dt)
  (unless (pinned-p verlet)
    (let* ((vel (nv* (velocity verlet) (friction verlet)))
           (pos (location verlet)))
      (when (and (< 0.000001 (vlength vel)) (not (constrained-p verlet)))
        (nv* vel (ground-friction verlet)))
      (nv+ vel (gravity verlet))
      (nv* vel dt)
      (setf (old-location verlet) pos)
      (nv+ pos vel))))
