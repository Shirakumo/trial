#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass clip (sequences:sequence)
  ((name :initarg :name :initform NIL :accessor name)
   (tracks :initform #() :accessor tracks)
   (start-time :initform 0.0 :accessor start-time)
   (end-time :initform 0.0 :accessor end-time)
   (loop-p :initarg :loop-p :initform T :accessor loop-p)))

(defun fit-to-clip (clip time)
  (let ((frames (frames track)))
    (let ((start (start-time clip))
          (end (end-time clip)))
      (if loop-p
          (+ start (mod (- time start) (- end start)))
          (clamp start time end)))))

(defun recompute-duration (clip)
  (loop for track across (tracks clip)
        when (valid-p track) minimize (start-time track) into start
        when (valid-p track) maximize (end-time track) into end
        finally (setf (start-time clip) (float start 0.0)
                      (end-time clip) (float end 0.0))))

(defmethod (setf tracks) :after (tracks (clip clip))
  (recompute-duration clip))

(defmethod sequences:length ((clip clip))
  (length (tracks clip)))

(defmethod sequences:adjust-sequence ((clip clip) length &rest args)
  (setf (tracks clip) (apply #'adjust-array (tracks clip) length args))
  clip)

(defmethod sequences:elt ((clip clip) index)
  (svref (tracks clip) index))

(defmethod (setf sequences:elt) (track (clip clip) index)
  (setf (svref (tracks clip) index) track)
  (recompute-duration clip)
  track)

(defmethod find-track ((clip clip) name &key (if-does-not-exist :error))
  (loop for track across (tracks clip)
        do (when (eql name (name track))
             (return track))
        finally (ecase if-does-not-exist
                  (:error (error "No track with name ~s found." name))
                  (:create (let ((track (make-instance 'transform-track :name name)))
                             (adjust-array clip (1+ (length clip)) :initial-element track)
                             (return track)))
                  ((NIL) (return NIL)))))

(defmethod sample-pose ((clip clip) pose time)
  (let ((time (fit-to-clip clip time))
        (tracks (tracks clip))
        (loop-p (loop-p clip)))
    (loop for i from 0 below (length tracks)
          for track = (svref tracks i)
          do (sample-transform track (elt pose (name track)) time loop-p))))
