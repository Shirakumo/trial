#|
This file is a part of trial
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defparameter +edge-default-width+ 1)
(defparameter +edge-default-stiffness+ 1.88)

(define-asset (physics box) mesh
  (make-cube +mesh-size+))

(define-shader-entity edge (vertex-entity colored-entity scaled-entity rotated-entity located-entity)
  ((target :initarg :target :accessor target)
   (width :initarg :width :accessor width)
   (verlet-a :initarg :verlet-a :accessor verlet-a)
   (verlet-b :initarg :verlet-b :accessor verlet-b)
   (stiffness :initarg :stiffness :accessor stiffness))
  (:default-initargs :color (vec4 1 0 0 1)
                     :vertex-array (// 'physics 'box)
                     :target NIL
                     :width +edge-default-width+
                     :stiffness +edge-default-stiffness+
                     :verlet-a (error "VERLET-A required")
                     :verlet-b (error "VERLET-B required")))

(defmethod initialize-instance :after ((edge edge) &key)
  (let ((target (or (target edge) (magnitude edge))))
    (setf (target edge) target)
    (update-location edge)
    (update-rotation edge)
    (update-scaling edge)))

(defmethod update-location ((edge edge))
  (let ((pos-a (location (verlet-a edge)))
        (pos-b (location (verlet-b edge))))
    (if (v= pos-a pos-b)
        (setf (location edge) (vcopy pos-a))
        (let* ((half (/ (vdistance pos-a pos-b) 2))
               (direction (nvunit (v- pos-b pos-a)))
               (loc (v+ (location (verlet-a edge)) (nv* direction half))))
          (setf (location edge) loc))))
  (location edge))

(defmethod update-rotation ((edge edge))
  (let* ((pos-a (location (verlet-a edge)))
         (pos-b (location (verlet-b edge)))
         (delta (v- pos-b pos-a))
         (angle (if (v/= pos-a pos-b) (vangle +vx3+ delta) 0)))
    (when (< (vy delta) 0) (incf angle (* 2 (- pi angle))))
    (setf (vz (rotation edge)) angle))
  (rotation edge))

(defmethod update-scaling ((edge edge))
  (setf (vx (scaling edge)) (/ (magnitude edge) +mesh-size+))
  (setf (vy (scaling edge)) (/ (width edge) +mesh-size+))
  (setf (vz (scaling edge)) 0.01)
  (scaling edge))

(defmethod proportions ((edge edge))
  (let ((proportions (v* (scaling edge) +mesh-size+)))
    (setf (vz proportions) 0)
    proportions))

(defmethod magnitude ((edge edge))
  (vdistance (location (verlet-a edge)) (location (verlet-b edge))))

(defmethod simulate ((edge edge) dt)
  (let* ((v-a (verlet-a edge))
         (v-b (verlet-b edge))
         (pos-a (location v-a))
         (pos-b (location v-b))
         (delta (v- pos-b pos-a))
         (distance (vlength delta)))
    (unless (< 0.0 distance)
      (setf distance 0.000001)
      (setf delta (v* +vx3+ distance)))
    (let* ((difference (* (/ (- (target edge) distance) distance) (stiffness edge)))
           (offset (v* delta difference))
           (mass-total (+ (mass v-a) (mass v-b)))
           (mass-a (cond
                     ((pinned-p v-a) 0)
                     ((pinned-p v-b) 1)
                     (T (/ (mass v-b) mass-total))))
           (mass-b (cond
                     ((pinned-p v-b) 0)
                     ((pinned-p v-a) 1)
                     (T (/ (mass v-a) mass-total)))))
      (nv- pos-a (v* offset mass-a dt))
      (nv+ pos-b (v* offset mass-b dt))))
  (update-location edge)
  (update-rotation edge)
  (update-scaling edge))
