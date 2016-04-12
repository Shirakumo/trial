#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (flare:scene event-loop entity)
  ())

(defmethod enter :after ((subject subject) (scene scene))
  (add-handler subject scene))

(defmethod leave :after ((subject subject) (scene scene))
  (remove-handler subject scene))

(defmethod save-form ((scene scene))
  (let ((form `(progn)))
    (flare-indexed-set:do-set (entity (objects scene) (nreverse form))
      (let ((inner (save-form entity)))
        (when inner (push inner form))))))
