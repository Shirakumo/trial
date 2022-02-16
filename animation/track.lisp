#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defstruct (frame
            (:constructor make-frame (time curve)))
  (time 0.0 :type single-float)
  (curve NIL :type (function (single-float) T)))

(defclass track (sequences:sequence)
  ((frames :initarg :frames :initform #() :accessor frames)
   (interpolation :initarg :interpolation :initform :linear :accessor interpolation)))

(defmethod initialize-instance :after ((track track) &key keyframes)
  (setf (frames track) keyframes))

(defgeneric start-time (track))
(defgeneric end-time (track))
(defgeneric sample (track time loop-p))
(defgeneric find-frame-idx (track time loop-p))

(defmethod (setf frames) ((keyframes cons) (track track))
  (let ((frames (make-array (length keyframes))))
    (loop for i from 0
          for current = (first keyframes) then next
          for next in (rest keyframes)
          do (setf (aref frames i)
                   (make-frame (first current)
                               (ecase (interpolation track)
                                 (:constant (constant (second current)))
                                 (:linear (linear (second current) (second next)))
                                 (:hermite (hermite (second current) (third current)
                                                    (second next) (fourth current)))
                                 (:bezier (bezier (second current) (third current)
                                                  (second next) (fourth current)))))))
    (setf (frames track) frames)))

(defun fit-to-track (track time loop-p)
  (let ((frames (frames track)))
    (if (<= (length frames) 1)
        0.0
        (let ((start (frame-time (svref frames 0)))
              (end (frame-time (svref frames (1- (length frames))))))
          (if loop-p
              (+ start (mod (- time start) (- end start)))
              (clamp start time end))))))

(defmethod start-time ((track track))
  (frame-time (svref (frames track) 0)))

(defmethod end-time ((track track))
  (frame-time (svref (frames track) (1- (length (frames track))))))

(defmethod sequences:adjust-sequence ((track track) length &rest args)
  (setf (frames track) (apply #'adjust-array (frames track) length args))
  track)

(defmethod sequences:length ((track track))
  (length (frames track)))

(defmethod sequences:elt ((track track) index)
  (svref (frames track) index))

(defmethod (setf sequences:elt) (value (track track) index)
  (setf (svref (frames track) index) value))

(defmethod find-frame-idx ((track track) x loop-p)
  (let ((x (fit-to-track track x loop-p))
        (frames (frames track)))
    (loop for i from 0 below (length frames)
          do (when (<= x (frame-time (svref frames i)))
               (return (1- i))))))

(defmethod sample ((track track) time loop-p)
  (let* ((frames (frames track))
         (i (find-frame-idx track time loop-p))
         (l (svref frames i))
         (r (svref frames (1+ i)))
         (x (/ (- time (frame-time l))
               (- (frame-time r) (frame-time l)))))
    (funcall (frame-curve l) x)))

(defclass transform-track ()
  ((name :initarg :name :initform NIL :accessor name)
   (location :initform (make-instance 'track) :accessor location)
   (scaling :initform (make-instance 'track) :accessor scaling)
   (rotation :initform (make-instance 'track) :accessor rotation)))

(defmethod start-time ((track transform-track))
  (min (start-time (location track))
       (start-time (scaling track))
       (start-time (rotation track))))

(defmethod end-time ((track transform-track))
  (max (end-time (location track))
       (end-time (scaling track))
       (end-time (rotation track))))

(defmethod sample-transform ((track transform-track) transform time loop-p)
  (when (< 1 (length (location track)))
    (setf (tlocation transform) (sample (location track) time loop-p)))
  (when (< 1 (length (scaling track)))
    (setf (tscaling transform) (sample (scaling track) time loop-p)))
  (when (< 1 (length (rotation track)))
    (setf (trotation transform) (sample (rotation track) time loop-p))))

(defmethod valid-p ((track transform-track))
  (or (< 1 (length (location track)))
      (< 1 (length (location track)))
      (< 1 (length (location track)))))
