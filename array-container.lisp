#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass array-container (container)
  ((objects :initform (make-array 0 :adjustable T :fill-pointer T) :accessor objects)))

(defmethod flare:update ((container array-container) dt)
  (for:for ((item across (objects container)))
    (update item dt)))

(defmethod enter (thing (container array-container))
  (vector-push-extend thing (objects container))
  thing)

(defmethod leave (thing (container array-container))
  (array-utils:vector-pop-position (objects container)
                                   (position thing (objects container)))
  thing)

(defmethod clear ((container array-container))
  (let ((objects (objects container)))
    (loop for i from 0 below (length objects)
          do (setf (aref objects i) NIL))
    (adjust-array objects 0 :fill-pointer 0))
  container)

(defmethod unit (n (container array-container))
  (aref (objects container) n))

(defmethod (setf unit) (value n (container array-container))
  (setf (aref (objects container) n) value))

(defmethod finalize ((container array-container))
  (for:for ((object across (objects container)))
    (finalize object)))

(defmethod enter* ((thing entity) (container array-container))
  (enter thing container)
  (if (= 1 (length (objects container)))
      (compile-into-pass thing NIL *scene*)
      (compile-into-pass thing (aref (objects container) (- (length (objects container)) 2)) *scene*)))
