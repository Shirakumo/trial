#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (flare:scene event-loop entity)
  ())

(defclass scene-event (event)
  ((scene :initarg :scene :accessor scene)))

(defclass enter (scene-event)
  ((entity :initarg :entity :accessor entity)))

(defclass leave (scene-event)
  ((entity :initarg :entity :accessor entity)))

(defmethod enter :after ((entity entity) (scene scene))
  (issue scene 'enter :scene scene :entity entity))

(defmethod leave :after ((entity entity) (scene scene))
  (issue scene 'leave :scene scene :entity entity))

(defmethod enter :after ((container handler-container) (scene scene))
  (add-handler container scene))

(defmethod leave :after ((container handler-container) (scene scene))
  (remove-handler container scene))

(defmethod enter :after ((container container) (scene scene))
  (let ((handlers ()))
    (flare-indexed-set:do-set (thing (objects container))
      (when (typep thing 'handler-container)
        (setf handlers (nconc handlers (handlers thing)))))
    ;; Bulk update
    (add-handler handlers scene)))

(defmethod leave :after ((container container) (scene scene))
  (let ((handlers ()))
    (flare-indexed-set:do-set (thing (objects container))
      (when (typep thing 'handler-container)
        (setf handlers (nconc handlers (handlers thing)))))
    ;; Bulk update
    (remove-handler handlers scene)))

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

(defmethod finalize :after ((scene scene))
  (clear scene))
