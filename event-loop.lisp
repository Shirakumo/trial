#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *loop*)

(defgeneric add-handler (handler handler-container))
(defgeneric remove-handler (handler handler-container))
(defgeneric handle (event handler))

(defclass handler-container ()
  ((handlers :initform () :accessor handlers)))

(defmethod add-handler (handler (container handler-container))
  (setf (handlers container)
        (sort (cons handler (delete handler (handlers container) :test #'matches))
              #'> :key #'priority)))

(defmethod add-handler ((handlers list) (container handler-container))
  (let ((handlers (copy-list handlers)))
    (loop for cons on (handlers container)
          for handler = (find (car cons) handlers :test #'matches)
          do (when handler
               (setf (car cons) handler)
               (setf handlers (delete handler handlers))))
    (setf (handlers container) (sort (nconc handlers (handlers container)) #'> :key #'priority))))

(defmethod add-handler ((source handler-container) (container handler-container))
  (add-handler (handlers source) container))

(defmethod remove-handler (handler (container handler-container))
  (setf (handlers container)
        (delete handler (handlers container) :test #'matches)))

(defmethod remove-handler ((handlers list) (container handler-container))
  (setf (handlers container) (delete-if (lambda (el)
                                          (find el handlers :test #'matches))
                                        (handlers container))))

(defmethod remove-handler ((source handler-container) (container handler-container))
  (remove-handler (handlers source) container))

(defmethod handle (event (container handler-container))
  (dolist (handler (handlers container))
    (handle event handler)))

(defmethod handle :around (event handler)
  (with-simple-restart (abort "Don't handle ~a in ~a." event handler)
    (call-next-method)))

(defclass event-loop (handler-container)
  ((queue :initform (make-array 0 :initial-element NIL :adjustable T :fill-pointer T) :reader queue)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-instance event-type args)))))
    (vector-push-extend event (queue loop))))

(defun process (loop)
  (loop for i from 0
        while (< i (length (queue loop)))
        do (handle (aref (queue loop) i) loop)
           (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0))

(defmethod handle (event (loop event-loop))
  (let ((*loop* loop))
    (call-next-method)))

(defclass handler (entity)
  ((event-type :initarg :event-type :accessor event-type)
   (container :initarg :container :accessor container)
   (delivery-function :initarg :delivery-function :accessor delivery-function)
   (priority :initarg :priority :accessor priority))
  (:default-initargs
   :event-type (error "EVENT-TYPE required.")
   :container (error "CONTAINER required.")
   :delivery-function (error "DELIVERY-FUNCTION needed.")
   :priority 0))

(defmethod matches ((a handler) (b handler))
  (and (eq (container a) (container b))
       (eql (name a) (name b))))

(defmethod handle (event (handler handler))
  (when (typep event (event-type handler))
    (funcall (delivery-function handler) (container handler) event)))

(defclass event ()
  ())

(defclass tick (event)
  ((tick-count :initarg :tick-count :reader tick-count)))
