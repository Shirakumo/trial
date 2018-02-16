#|
 This file is a part of trial
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: configurable defaults

(defclass resource ()
  ())

(defgeneric allocate (resource))
(defgeneric deallocate (resource))
(defgeneric allocated-p (resource))

(defmethod allocate :around ((resource resource))
  (call-next-method)
  resource)

(defmethod deallocate :around ((resource resource))
  (call-next-method)
  resource)

(defun check-allocated (resource)
  (unless (allocated-p resource)
    (error "~s is not yet allocated." resource)))

(defclass foreign-resource (resource)
  ((data-pointer :initform NIL :initarg :data-pointer :accessor data-pointer)))

(defgeneric destructor (foreign-resource))

(defmethod destructor ((resource foreign-resource))
  (lambda ()))

(defmethod allocated-p ((resource foreign-resource))
  (data-pointer resource))

(defmethod allocate :after ((resource foreign-resource))
  (tg:finalize resource (destructor resource)))

(defmethod deallocate :after ((resource foreign-resource))
  (tg:cancel-finalization resource)
  (setf (data-pointer resource) NIL))

(defclass gl-resource (foreign-resource)
  ((data-pointer :reader gl-name)))
