#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject player (located-subject mesh-subject)
  ((velocity :initarg :velocity :accessor velocity))
  (:default-initargs
   :mesh "cube.obj"
   :velocity (vec 0 0 0)
   :location (vec 0 0 0)
   :name :player))

(define-handler (player tick) (ev)
  (nv+ (location player) (velocity player)))

(define-handler (player movement) (ev)
  (typecase ev
    (start-left (setf (vx (velocity player)) -5))
    (start-right (setf (vx (velocity player)) 5))
    (start-up (setf (vy (velocity player)) 5))
    (start-down (setf (vy (velocity player)) -5))
    (stop-left (when (< (vx (velocity player)) 0)
                 (setf (vx (velocity player)) 0)))
    (stop-right (when (< 0 (vx (velocity player)))
                  (setf (vx (velocity player)) 0)))
    (stop-up (when (< 0 (vy (velocity player)))
               (setf (vy (velocity player)) 0)))
    (stop-down (when (< (vy (velocity player)) 0)
                 (setf (vy (velocity player)) 0)))))
