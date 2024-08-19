(in-package #:org.shirakumo.fraf.trial)

;; FIXME: configurable defaults

(defclass loadable () ())

(defclass resource (loadable)
  ((generator :initarg :generator :initform NIL :reader generator)
   (name :initarg :name :initform NIL :reader name)
   (dependencies :initarg :dependencies :initform () :accessor dependencies)))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type T :identity T)
    (format stream "~@[~a~]~@[ ~a~]~:[~; ALLOCATED~]" (generator resource) (name resource) (allocated-p resource))))

(defgeneric allocate (resource))
(defgeneric deallocate (resource))
(defgeneric allocated-p (resource))

(defmethod load ((resource resource))
  (unless (allocated-p resource)
    (v:trace :trial.resource "Loading ~a" resource)
    (allocate resource)))

(defmethod allocate :around ((resource resource))
  #-elide-allocation-checks
  (when (allocated-p resource)
    (error "~a is already loaded!" resource))
  #-trial-release
  (v:trace :trial.resource "Allocating ~a" resource)
  (call-next-method)
  resource)

(defmethod deallocate :around ((resource resource))
  #-elide-allocation-checks
  (unless (allocated-p resource)
    (error "~a is not loaded!" resource))
  #-trial-release
  (v:trace :trial.resource "Deallocating ~a" resource)
  (call-next-method)
  resource)

(defmethod loaded-p ((resource resource))
  (allocated-p resource))

(defmethod finalize ((resource resource))
  (when (allocated-p resource)
    (deallocate resource)))

(defun check-allocated (resource)
  (unless (allocated-p resource)
    (restart-case
        (error 'resource-not-allocated :resource resource)
      (continue ()
        :report "Allocate the resource now and continue."
        (allocate resource)))))

(defun ensure-allocated (resource)
  (unless (allocated-p resource)
    (mapc #'ensure-allocated (dependencies resource))
    (handler-bind ((resource-not-allocated #'continue))
      (allocate resource)))
  resource)

(defclass foreign-resource (resource)
  ((data-pointer :initform NIL :initarg :data-pointer :accessor data-pointer)))

(defmethod allocated-p ((resource foreign-resource))
  (data-pointer resource))

(defmethod deallocate :after ((resource foreign-resource))
  (setf (data-pointer resource) NIL))

(defclass gl-resource (foreign-resource)
  ((data-pointer :accessor gl-name)))

(defgeneric activate (gl-resource))
(defgeneric deactivate (gl-resource))

#-elide-context-current-checks
(defmethod allocate :before ((resource gl-resource))
  (check-context-current))

#-elide-context-current-checks
(defmethod deallocate :before ((resource gl-resource))
  (check-context-current))

#-elide-context-current-checks
(defmethod activate :before ((resource gl-resource))
  (check-context-current))

#-elide-context-current-checks
(defmethod deactivate :before ((resource gl-resource))
  (check-context-current))
