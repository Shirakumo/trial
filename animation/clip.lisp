#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(defclass clip (sequences:sequence standard-object)
  ((name :initarg :name :initform NIL :accessor trial:name)
   (tracks :initform #() :accessor tracks)
   (start-time :initform 0.0 :accessor start-time)
   (end-time :initform 0.0 :accessor end-time)
   (loop-p :initarg :loop-p :initform T :accessor loop-p)))

(defmethod initialize-instance :after ((clip clip) &key tracks)
  (when tracks
    (setf (tracks clip) tracks)))

(defmethod print-object ((clip clip) stream)
  (print-unreadable-object (clip stream :type T)
    (format stream "~s ~a ~a" (trial:name clip) (start-time clip) (end-time clip))))

(defun fit-to-clip (clip time)
  (let ((start (start-time clip))
        (end (end-time clip)))
    (if (loop-p clip)
        (+ start (mod (- time start) (- end start)))
        (trial:clamp start time end))))

(defun recompute-duration (clip)
  (loop for track across (tracks clip)
        when (valid-p track) minimize (start-time track) into start
        when (valid-p track) maximize (end-time track) into end
        finally (setf (start-time clip) (float start 0.0)
                      (end-time clip) (float end 0.0)))
  clip)

(defmethod duration ((clip clip))
  (- (end-time clip) (start-time clip)))

(defmethod (setf tracks) :after (tracks (clip clip))
  (recompute-duration clip))

(defmethod sequences:length ((clip clip))
  (length (tracks clip)))

(defmethod sequences:adjust-sequence ((clip clip) length &rest args)
  (declare (ignore args))
  (let* ((tracks (tracks clip))
         (old (length tracks)))
    (setf tracks (adjust-array tracks length))
    (loop for i from old below length
          do (setf (svref tracks i) (make-instance 'transform-track)))
    (setf (tracks clip) tracks))
  clip)

(defmethod sequences:elt ((clip clip) index)
  (svref (tracks clip) index))

(defmethod (setf sequences:elt) (track (clip clip) index)
  (setf (svref (tracks clip) index) track)
  (recompute-duration clip)
  track)

(defmethod find-track ((clip clip) name &key (if-does-not-exist :error))
  (loop for track across (tracks clip)
        do (when (eql name (trial:name track))
             (return track))
        finally (ecase if-does-not-exist
                  (:error (error "No track with name ~s found." name))
                  (:create (progn 
                             (sequences:adjust-sequence clip (1+ (length clip)))
                             (let ((track (elt clip (1- (length clip)))))
                               (setf (trial:name track) name)
                               (return track))))
                  ((NIL) (return NIL)))))

(defmethod sample-pose ((clip clip) pose time)
  (if (< 0.0 (end-time clip))
      (let ((time (fit-to-clip clip time))
            (tracks (tracks clip))
            (loop-p (loop-p clip)))
        (loop for i from 0 below (length tracks)
              for track = (svref tracks i)
              do (sample-transform track (elt pose (trial:name track)) time loop-p))
        time)
      0.0))

(defmethod reorder ((clip clip) map)
  (dotimes (i (length clip) clip)
    (let ((track (elt clip i)))
      (setf (trial:name track) (gethash (trial:name track) map)))))
