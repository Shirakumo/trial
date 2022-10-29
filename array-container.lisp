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

(defmethod sequences:make-sequence-iterator ((container array-container) &rest args)
  (apply #'sequences:make-sequence-iterator (%objects container) args))

(defmethod for:make-iterator ((container array-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
