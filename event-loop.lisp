#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *event-loop* NIL)

(defclass synchronous-event-loop (deeds:compiled-event-loop)
  ((queue :initform (make-array 0 :element-type 'deeds:event :adjustable T :fill-pointer T) :reader queue)))

(defmethod deeds:issue ((event deeds:event) (loop synchronous-event-loop))
  (vector-push-extend event (queue loop)))

(defmethod deeds:start ((loop synchronous-event-loop))
  (setf (fill-pointer (queue loop)) 0)
  loop)

(defmethod deeds:stop ((loop synchronous-event-loop))
  loop)

(defmacro define-handler ((name event-type) args &body body)
  `(deeds:define-handler (,name ,event-type) ,args
     :class 'deeds:locally-blocking-handler
     :loop *event-loop*
     ,@body))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  `(deeds:define-event ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defun issue (event-type &rest args)
  (deeds:issue (apply #'make-instance event-type args) *event-loop*))

(defun process (loop)
  (loop for i from 0
        while (< i (length (queue loop)))
        do (handle (aref (queue loop) i) loop)
           (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0))

(define-event trial-event ()
  ())

(define-event tick (trial-event)
  ())

(define-event input-event (trial-event)
  ())

(define-event keyboard-event (input-event)
  ((key :initarg :key :reader key))
  (:default-initargs
   :key (error "KEY required.")))

(define-event key-press (keyboard-event)
  ())

(define-event key-release (keyboard-event)
  ())

(define-event mouse-event (input-event)
  ((button :initarg :button :reader button))
  (:default-initargs
   :button (error "BUTTON required.")))

(define-event mouse-button-pressed (mouse-event)
  ())

(define-event mouse-button-released (mouse-event)
  ())

(define-event mouse-move-event (input-event)
  ((old-pos :initarg :old-pos :reader old-pos)
   (new-pos :initarg :new-pos :reader new-pos))
  (:default-initargs
   :old-pos (error "OLD-POS required.")
   :new-pos (error "NEW-POS required.")))
