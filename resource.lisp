#|
 This file is a part of trial
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: configurable defaults

(define-condition resource-not-allocated (error)
  ((resource :initarg :resource))
  (:report (lambda (c s) (format s "The resource~%  ~s~%is required to be allocated, but was not yet."
                                 (slot-value c 'resource)))))

(defclass resource ()
  ())

(defgeneric allocate (resource))
(defgeneric deallocate (resource))
(defgeneric allocated-p (resource))

(defmethod load ((resource resource))
  (unless (allocated-p resource)
    (v:trace :trial.resource "Loading ~a" resource)
    (allocate resource)))

(defmethod allocate :around ((resource resource))
  (call-next-method)
  resource)

(defmethod deallocate :around ((resource resource))
  (call-next-method)
  resource)

(defmethod allocate :after ((resource resource))
  (setf (gethash resource (resources *context*)) resource))

(defmethod deallocate :after ((resource resource))
  (remhash resource (resources *context*)))

(defun check-allocated (resource)
  (unless (allocated-p resource)
    (restart-case
        (error 'resource-not-allocated :resource resource)
      (continue ()
        :report "Allocate the resource now and continue."
        (allocate resource)))))

(defclass foreign-resource (resource)
  ((data-pointer :initform NIL :initarg :data-pointer :accessor data-pointer)))

(defgeneric destructor (foreign-resource))

(defmethod destructor ((resource foreign-resource))
  (lambda ()))

(defmethod allocated-p ((resource foreign-resource))
  (data-pointer resource))

(defmethod allocate :after ((resource foreign-resource))
  (tg:finalize resource (destructor resource)))

(defmethod deallocate ((resource foreign-resource))
  (funcall (destructor resource)))

(defmethod deallocate :after ((resource foreign-resource))
  (tg:cancel-finalization resource)
  (setf (data-pointer resource) NIL))

(defclass gl-resource (foreign-resource)
  ((data-pointer :accessor gl-name)))
