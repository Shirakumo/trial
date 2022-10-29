#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass list-container (container)
  ((%objects :initform () :accessor %objects)))

(defmethod clear ((container list-container))
  (let ((objects (%objects container)))
    (loop while objects
          do (setf (container (pop objects)) NIL))
    (setf (%objects container) objects))
  container)

(defmethod enter (thing (container list-container))
  (push thing (%objects container))
  thing)

(defmethod leave (thing (container list-container))
  (setf (%objcets container) (delete thing (%objects container)))
  thing)

(defmethod finalize ((container list-container))
  (for:for ((object in (%objects container)))
           (finalize object)))

(defmethod sequences:elt ((container list-container) index)
  (nth index (%objects container)))

(defmethod (setf sequences:elt) (thing (container list-container) index)
  (setf (nth index (%objects container)) thing))

(defmethod sequences:make-sequence-iterator ((container list-container) &rest args)
  (apply #'sequences:make-sequence-iterator (%objects container) args))

(defmethod for:make-iterator ((container list-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
