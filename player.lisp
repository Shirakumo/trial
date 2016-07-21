#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject player (face-entity collidable-entity selectable-entity)
  ((velocity :initarg :velocity :accessor velocity))
  (:default-initargs
   :velocity (vec 0 0 0)
   :location (vec 0 0 0)
   :bounds (vec 20 20 0)
   :texture '(trial cat)
   :name :player))

(define-handler (player tick) (ev)
  (setf (vx (velocity player))
        (cond ((retained 'movement :left) -5)
              ((retained 'movement :right) 5)
              (T 0))
        (vz (velocity player))
        (cond ((retained 'movement :up) -5)
              ((retained 'movement :down) 5)
              (T 0)))
  
  (when (< 0 (vy (location player)))
    (decf (vy (velocity player)) 0.5))
  
  (nv+ (location player) (velocity player))

  (when (< (vy (location player)) 0)
    (setf (vy (location player)) 0)
    (setf (vy (velocity player)) 0)))

(define-handler (player perform) (ev)
  (when (= 0 (vy (location player)))
    (setf (vy (velocity player)) 5)))
