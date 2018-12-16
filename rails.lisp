#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-subject rail (clocked-subject)
  ((target :initarg :target :accessor target)
   (rail-points :accessor rail-points)
   (duration :initarg :duration :accessor duration))
  (:default-initargs
   :target NIL
   :rail-points (error "RAIL-POINTS required.")))

(defmethod initialize-instance :after ((rail rail) &key rail-points)
  (setf (rail-points rail) (coerce rail-points 'vector)))

(define-handler (rail tick) (ev)
  (when (and (target rail) (running rail))
    (setf (location (target rail))
          (rail-location rail (min 1 (/ (clock rail) (duration rail)))))))

(defgeneric rail-location (rail x))

(define-subject linear-rail (rail)
  ((rail-times :accessor rail-times)))

(defmethod (setf rail-points) :after (points (rail linear-rail))
  (let ((total (loop for i from 1 below (length points)
                     sum (vlength (v- (aref points i) (aref points (1- i))))))
        (times (make-array (length points) :element-type 'single-float
                                           :initial-element 0.0f0)))
    (loop for i from 1 below (length points)
          for v = (v- (aref points i) (aref points (1- i)))
          for d = (vlength v) then (+ d (vlength v))
          do (setf (aref times i) (/ d total)))
    (setf (rail-times rail) times)))

(defmethod rail-location (rail x)
  (let ((times (rail-times rail))
        (points (rail-points rail)))
    (loop for i from 1 below (length times)
          for ptime = (aref times 0) then time
          for time = (aref times i)
          do (when (<= x time)
               (let ((x (/ (- x ptime) (- time ptime))))
                 (return (nv+ (nv* (v- (aref points i) (aref points (1- i))) x)
                              (aref points (1- i))))))
          finally (return (aref points (1- (length points)))))))
