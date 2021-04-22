#|
This file is a part of trial
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defclass verlet-entity (entity listener)
  ((verlets :initform (make-array 16 :adjustable T :fill-pointer 0)
            :accessor verlets)
   (edges :initform (make-array 16 :adjustable T :fill-pointer 0)
          :accessor edges)
   (iterations :initarg :iterations :accessor iterations))
  (:default-initargs :iterations 16))

(defmethod add-verlet ((obj verlet-entity) (verlet verlet))
  (vector-push-extend verlet (verlets obj)))

(defmethod add-verlets ((obj verlet-entity) locations &optional constraint)
  (loop for pos in locations
        for verlet = (make-instance 'verlet :location pos :constraint constraint)
        do (add-verlet obj verlet)))

(defmethod add-edge ((obj verlet-entity) (edge edge))
  (vector-push-extend edge (edges obj)))

(defmethod add-edges ((obj verlet-entity) indices)
  (loop for (index-a index-b) on indices by #'cddr
        for verlet-a = (verlet obj index-a)
        for verlet-b = (verlet obj index-b)
        for edge = (make-instance 'edge :verlet-a verlet-a :verlet-b verlet-b)
        do (add-edge obj edge)))

(defmethod update-verlets ((obj verlet-entity) dt)
  (loop for verlet across (verlets obj)
        do (simulate verlet dt)))

(defmethod update-edges ((obj verlet-entity) dt)
  (loop for edge across (edges obj)
        do (simulate edge dt)))

(defmethod constrain ((obj verlet-entity))
  (loop for verlet across (verlets obj)
        do (constrain verlet)))

(defmethod verlet ((obj verlet-entity) index)
  (aref (verlets obj) index))

(defmethod edge ((obj verlet-entity) index)
  (aref (edges obj) index))

(defmethod enter :around ((obj verlet-entity) (scene scene-graph))
  (loop for verlet across (verlets obj)
        unless (slot-boundp verlet 'container)
        do (enter verlet scene))
  (loop for edge across (edges obj)
        unless (slot-boundp edge 'container)
        do (enter edge scene))
  (call-next-method))

(defmethod leave :around ((obj verlet-entity) (scene scene-graph))
  (call-next-method)
  (loop for verlet across (verlets obj)
        when (slot-boundp verlet 'container)
        do (leave verlet scene))
  (loop for edge across (edges obj)
        when (slot-boundp edge 'container)
        do (leave edge scene)))

(define-handler (verlet-entity tick) (dt)
  ;; Default physics calculations expect 0.01 second delta time.
  (let ((dt (/ dt 0.01)))
    (update-verlets verlet-entity dt)
    (let ((iterations (iterations verlet-entity)))
      (dotimes (i iterations)
        (update-edges verlet-entity dt)
        (constrain verlet-entity)))))
