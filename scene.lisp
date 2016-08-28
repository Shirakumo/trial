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

(defmethod print-object ((enter enter) stream)
  (print-unreadable-object (enter stream :type T)
    (format stream "~a => ~a" (entity enter) (scene enter))))

(defclass leave (scene-event)
  ((entity :initarg :entity :accessor entity)))

(defmethod print-object ((leave leave) stream)
  (print-unreadable-object (leave stream :type T)
    (format stream "~a => ~a" (scene leave) (entity leave))))

(defmethod register :after ((entity entity) (scene scene))
  (issue scene 'enter :scene scene :entity entity))

(defmethod deregister :after ((entity entity) (scene scene))
  (issue scene 'leave :scene scene :entity entity))

(defmethod register :after ((container handler-container) (scene scene))
  (add-handler container scene))

(defmethod deregister :after ((container handler-container) (scene scene))
  (remove-handler container scene))

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

(defun funcall-in-scene (scene func &key bindings (return-values T))
  (with-execution (return-values event 'execute-request :func func :bindings bindings )
    (issue scene event)))

(defmacro with-body-in-scene ((scene &key bindings (return-values T)) &body body)
  `(funcall-in-scene ,scene (lambda () ,@body) :bindings ,bindings :return-values ,return-values))
