#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass clip (sequences:sequence standard-object)
  ((name :initarg :name :initform NIL :accessor name)
   (tracks :initform #() :accessor tracks)
   (start-time :initform 0.0 :accessor start-time)
   (end-time :initform 0.0 :accessor end-time)
   (loop-p :initarg :loop-p :initform T :accessor loop-p)))

(defmethod shared-initialize :after ((clip clip) slots &key tracks)
  (when tracks
    (setf (tracks clip) tracks)))

(defmethod print-object ((clip clip) stream)
  (print-unreadable-object (clip stream :type T)
    (format stream "~s ~a ~a" (name clip) (start-time clip) (end-time clip))))

(defun fit-to-clip (clip time)
  (let ((start (start-time clip))
        (end (end-time clip)))
    (if (loop-p clip)
        (+ start (mod (- time start) (- end start)))
        (clamp start time end))))

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

(defmethod find-animation-track ((clip clip) name &key (if-does-not-exist :error))
  (loop for track across (tracks clip)
        do (when (eql name (name track))
             (return track))
        finally (ecase if-does-not-exist
                  (:error (error "No track with name ~s found." name))
                  (:create (progn 
                             (sequences:adjust-sequence clip (1+ (length clip)))
                             (let ((track (elt clip (1- (length clip)))))
                               (setf (name track) name)
                               (return track))))
                  ((NIL) (return NIL)))))

(defmethod sample (pose (clip clip) time &key)
  (if (< 0.0 (end-time clip))
      (let ((time (fit-to-clip clip time))
            (tracks (tracks clip))
            (loop-p (loop-p clip)))
        (loop for i from 0 below (length tracks)
              for track = (svref tracks i)
              do (sample (elt pose (name track)) track time :loop-p loop-p))
        time)
      0.0))

(defmethod reorder ((clip clip) map)
  (dotimes (i (length clip) clip)
    (let ((track (elt clip i)))
      (setf (name track) (gethash (name track) map)))))

(defvar *clips* (make-hash-table :test 'equal))

(defmethod clip ((name symbol))
  (gethash name *clips*))

(defmethod (setf clip) (clip (name symbol))
  (setf (gethash name *clips*) clip))

(defmacro define-clip ((name &rest track-interpolations) &body frames)
  `(setf (clip ',name) (ensure-instance (clip ',name) 'clip :tracks (compile-tracks ',track-interpolations
                                                                                    ,@(loop for part in frames
                                                                                            collect (cond ((and (symbolp part) (string= "_" part))
                                                                                                           NIL)
                                                                                                          ((and (consp part) (not (symbolp (car part))))
                                                                                                           `(list ,@part))
                                                                                                          (T
                                                                                                           part)))))))

(defun compile-tracks (interpolations &rest frame-data)
  (let ((tracks (make-array (length interpolations))))
    (loop for i from 0
          for interpolation in interpolations
          for track-frames = (loop for frame = frame-data then (nthcdr (1+ (length interpolations)) frame)
                                   for value = (nth (1+ i) frame)
                                   while frame
                                   when value
                                   collect (cons (car frame) value))
          for track = (make-instance 'animation-track
                                     :interpolation interpolation
                                     :times (mapcar #'car track-frames)
                                     :values (let ((values (mapcar #'cdr track-frames)))
                                               ;; Duplicate last frame values to ensure we leave enough data for the track to finish.
                                               (setf (cdr (last values)) (cons (car (last values)) (last values)))
                                               (alexandria:flatten values)))
          do (setf (aref tracks i) track))
    tracks))

#++
(define-clip (foo :linear :hermite)
  0.0 (vec 3 2 1) (1.0 2.0 0.0)
  1.0 _           (2.0 3.0 0.0)
  2.0 (vec 0 2 1) _)
