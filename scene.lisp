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

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister :after (thing (scene scene))
  (remove-listener thing scene))

(defmethod paint :around ((scene scene) target)
  (let ((*scene* scene))
    (call-next-method)))

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
