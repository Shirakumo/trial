#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass execute-request (event)
  ((func :initarg :func :reader func)
   (bindings :initarg :bindings :reader bindings)
   (result :initform NIL :accessor result))
  (:default-initargs
   :func (error "FORM required.")
   :bindings ()))

(defmethod execute :around ((request execute-request))
  (with-slots (bindings result) request
    (handler-case
        (with-error-logging (:trial.executable "Failed to execute ~a" request)
          (progv (mapcar #'first bindings)
              (mapcar #'second bindings)
            (setf result (cons :success (multiple-value-list (call-next-method))))))
      (error (err)
        (setf result (cons :failure err))))))

(defmethod execute ((request execute-request))
  (funcall (func request)))

(defmacro with-execution ((return-values eventvar execute-type &rest args) &body body)
  `(let ((,eventvar (make-instance ,execute-type ,@args)))
     ,@body
     (when ,return-values
       (values-list
        (loop for result = (result event)
              do (bt:thread-yield)
                 (case (car result)
                   (:failure (error (cdr result)))
                   (:success (return (cdr result)))))))))

(define-widget executable (QObject)
  ((execute-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor execute-queue)))

(define-signal (executable execute) ())

(define-slot (executable execute) ()
  (declare (connected executable (execute)))
  (loop for ev across execute-queue
        do (v:debug :trial.executable "~a is executing ~a" executable ev)
           (execute ev)
        finally (setf (fill-pointer execute-queue) 0)))

(defun funcall-in-gui (executable func &key bindings (return-values T))
  (with-execution (return-values event 'execute-request :func func :bindings bindings)
    (vector-push-extend event (execute-queue executable))
    (signal! executable (execute))))

(defmacro with-body-in-gui ((executable &key bindings (return-values T)) &body body)
  `(funcall-in-gui ,executable (lambda () ,@body) :bindings ,bindings :return-values ,return-values))
