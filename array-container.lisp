#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass array-container (container)
  ((%objects :initform (make-array 0 :adjustable T :fill-pointer T) :accessor %objects)))

(defmethod clear ((container array-container))
  (let ((objects (%objects container)))
    (loop for i from 0 below (length objects)
          do (setf (container (aref objects i)) NIL)
             (setf (aref objects i) NIL))
    (adjust-array objects 0 :fill-pointer 0))
  container)

(defmethod enter (thing (container array-container))
  (vector-push-extend thing (%objects container))
  thing)

(defmethod leave (thing (container array-container))
  (array-utils:vector-pop-position (%objects container)
                                   (position thing (%objects container)))
  thing)

(defmethod finalize ((container array-container))
  (for:for ((object across (%objects container)))
    (finalize object)))

(defmethod sequences:elt ((container array-container) index)
  (svref (%objects container) index))

(defmethod (setf sequences:elt) (thing (container array-container) index)
  (setf (svref (%objects container) index) thing))

(defmethod sequences:make-sequence-iterator ((container array-container) &key (start 0) end from-end)
  (let* ((vector (the (and vector (not simple-vector)) (%objects container)))
         (end (or end (length vector)))
         (iterator (if from-end (1- end) start))
         (limit (if from-end (1- start) end))
         (last-accessed NIL)
         (offset 0))
    (values iterator limit from-end
            (if from-end
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (unless (eq last-accessed (aref vector iterator))
                    (incf offset))
                  (1- iterator))
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (unless (eq last-accessed (aref vector iterator))
                    (decf offset))
                  (1+ iterator)))
            (lambda (sequence iterator limit from-end)
              (declare (ignore sequence from-end))
              (= iterator limit))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (setf last-accessed (aref vector (+ iterator offset))))
            (lambda (new-value sequence iterator)
              (declare (ignore sequence))
              (setf (aref vector (+ iterator offset)) new-value))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator)
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator))))

(defmethod for:make-iterator ((container array-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
