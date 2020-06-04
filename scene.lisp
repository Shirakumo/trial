#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: get rid of this shit or at least find a better way to do this stuff.
(defvar *scene*)

(defclass scene (flare:scene event-loop entity)
  ())

(defclass scene-event (event)
  ((scene :initarg :scene :reader scene)))

(defclass enter (scene-event)
  ((container :initarg :container :reader container)
   (entity :initarg :entity :reader entity)))

(defmethod print-object ((ev enter) stream)
  (print-unreadable-object (ev stream :type T)
    (format stream "~a => ~a" (entity ev) (container ev))))

(defclass leave (scene-event)
  ((container :initarg :container :reader container)
   (entity :initarg :entity :reader entity)))

(defmethod print-object ((ev leave) stream)
  (print-unreadable-object (ev stream :type T)
    (format stream "~a <= ~a" (entity ev) (container ev))))

(defmethod enter :after ((entity entity) (container container-unit))
  (when (scene-graph container)
    (issue (scene-graph container) 'enter :scene (scene-graph container) :container container :entity entity)))

(defmethod leave :after ((entity entity) (container container-unit))
  (when (scene-graph container)
    (issue (scene-graph container) 'leave :scene (scene-graph container) :container container :entity entity)))

(defmethod enter :after ((entity entity) (scene scene))
  (issue scene 'enter :scene scene :container scene :entity entity))

(defmethod leave :after ((entity entity) (scene scene))
  (issue scene 'leave :scene scene :container scene :entity entity))

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister :after (thing (scene scene))
  (remove-listener thing scene))

(defmethod process :around ((scene scene))
  (let ((*scene* scene))
    (call-next-method)))

(defmethod banned-slots append ((object scene))
  '(queue listeners))

;; Since we have a tick event, we don't want to dupe that here.
;; animations and clock update are already handled by the method
;; combination, but defining a noop primary method prevents update
;; from being called on the children.
(defmethod flare:update ((scene scene)))

;; But we still need to call it in tick.
(defmethod handle :before ((event tick) (scene scene))
  (flare:update scene))

(defmethod finalize :after ((scene scene))
  (stop scene)
  (clear scene))
