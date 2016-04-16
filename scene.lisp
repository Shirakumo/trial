#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (flare:scene event-loop entity)
  ())

(defmethod enter :after ((subject subject) (scene scene))
  (setf (alive-p subject) T)
  (add-handler subject scene))

(defmethod leave :after ((subject subject) (scene scene))
  (setf (alive-p subject) NIL)
  (remove-handler subject scene))

(defmethod save-form ((scene scene))
  (let ((form `(progn)))
    (flare-indexed-set:do-set (entity (objects scene) (nreverse form))
      (let ((inner (save-form entity)))
        (when inner (push inner form))))))

;; Since we have a tick event, we don't want to dupe that here.
;; animations and clock update are already handled by the method
;; combination, but defining a noop primary method prevents update
;; from being called on the children.
(defmethod update ((scene scene)))

;; But we still need to call it in tick.
(defmethod handle :before ((event tick) (scene scene))
  (update scene))
