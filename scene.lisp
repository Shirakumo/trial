#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene (flare:scene event-loop)
  ())

(defmethod register :after ((listener listener) (scene scene))
  (add-listener listener scene))

(defmethod deregister :after (thing (scene scene))
  (remove-listener thing scene))

(defmethod scene ((scene scene)) scene)

;; Since we have a tick event, we don't want to dupe that here.
;; animations and clock update are already handled by the method
;; combination, but defining a noop primary method prevents update
;; from being called on the children.
(defmethod flare:update ((scene scene) dt))

;; But we still need to call it in tick.
(defmethod handle :before ((event tick) (scene scene))
  (flare:update scene (dt event)))

(defmethod finalize :after ((scene scene))
  (stop scene)
  (clear scene))
