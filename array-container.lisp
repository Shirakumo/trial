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
          do (setf (aref objects i) NIL))
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
  (for:for ((object across (objects container)))
    (finalize object)))

(defmethod contains-p (thing (container array-container))
  (find thing (%objects container)))
