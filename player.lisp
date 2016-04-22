#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject player (face-entity collidable-entity selectable-entity savable)
  ((velocity :initarg :velocity :accessor velocity))
  (:default-initargs
   :velocity (vec 0 0 0)
   :location (vec 0 0 0)
   :texture '(trial cat)
   :name :player))

(define-handler (player tick) (ev)
  (nv+ (location player) (velocity player)))

(define-handler (player movement) (ev)
  (typecase ev
    (start-left (setf (vx (velocity player)) -5))
    (start-right (setf (vx (velocity player)) 5))
    (start-up (setf (vz (velocity player)) -5))
    (start-down (setf (vz (velocity player)) 5))
    (stop-left (when (< (vx (velocity player)) 0)
                 (setf (vx (velocity player)) 0)))
    (stop-right (when (< 0 (vx (velocity player)))
                  (setf (vx (velocity player)) 0)))
    (stop-up (when (< (vz (velocity player)) 0)
               (setf (vz (velocity player)) 0)))
    (stop-down (when (< 0 (vz (velocity player)))
                 (setf (vz (velocity player)) 0)))))
