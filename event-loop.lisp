#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass event ()
  ())

(defclass listener ()
  ())

(defgeneric add-listener (listener event-loop))
(defgeneric remove-listener (listener event-loop))
(defgeneric handle (event listener))

(defmethod handle :around ((event event) listener)
  (with-simple-restart (abort "Don't handle ~a in ~a." event listener)
    (call-next-method)))

;; Default to doing nothing.
(defmethod handle ((event event) (listener listener)))

(defclass event-loop ()
  ((queue :initform (make-array 64 :initial-element NIL :adjustable T :fill-pointer 0) :reader queue)
   (queue-index :initform 0 :accessor queue-index)
   (listeners :initform (make-hash-table :test 'eq) :accessor listeners)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-instance event-type args)))))
    (vector-push-extend event (queue loop))))

(define-compiler-macro issue (&environment env loop event-type &rest args)
  (cond ((and (constantp event-type env)
              (listp event-type)
              (eql (first event-type) 'quote)
              (symbolp (second event-type)))
         `(vector-push-extend (make-instance ,event-type ,@args) (queue ,loop)))
        (T
         (let ((eventg (gensym "EVENT")))
           `(let* ((,eventg ,event-type)
                   (,eventg (etypecase ,eventg
                              (event ,eventg)
                              ((or class symbol)
                               (make-instance ,eventg ,@args)))))
              (vector-push-extend ,eventg (queue ,loop)))))))

;; FIXME: This will forget events if PROCESS or DISCARD-EVENTS is called
;;        recursively (thus resetting the index) and new events are issued
;;        beyond the point of the index where the recursive call happens.
;;        The check will assume nothing has changed and it'll continue from
;;        where it left off, thus missing events before the current index.
(defmethod process ((loop event-loop))
  (with-simple-restart (discard-events "Discard all events.")
    (loop for i = (1- (incf (queue-index loop)))
          while (< i (length (queue loop)))
          do (let ((event (aref (queue loop) i)))
               (when event
                 (handle event loop)
                 (setf (aref (queue loop) i) NIL)))))
  (setf (fill-pointer (queue loop)) 0
        (queue-index loop) 0))

(defun discard-events (loop)
  (loop for i = (1- (incf (queue-index loop)))
        while (< i (length (queue loop)))
        do (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0
        (queue-index loop) 0))

(defmethod handle ((event event) (loop event-loop))
  (with-simple-restart (skip-event "Skip handling the event entirely.")
    (loop for listener being the hash-keys of (listeners loop)
          do (handle event listener))))

(defmethod add-listener (listener (loop event-loop))
  (setf (gethash listener (listeners loop)) listener))

(defmethod remove-listener (listener (loop event-loop))
  (remhash listener (listeners loop)))

(defmethod clear ((loop event-loop))
  (discard-events loop)
  (clrhash (listeners loop)))

(defmacro define-handler ((class event &rest qualifiers) slots &body body)
  (destructuring-bind (instance class) (enlist class class)
    (destructuring-bind (variable event) (enlist event event)
      `(defmethod handle ,@qualifiers ((,variable ,event) (,instance ,class))
         (let ,(loop for slot in slots
                     for (var name) = (enlist slot slot)
                     collect `(,var (slot-value ,variable ',name)))
           ,@body)))))

(defclass tick (event)
  ((tt :initarg :tt :accessor tt)
   (dt :initarg :dt :accessor dt)
   (fc :initarg :fc :accessor fc)))

(defclass class-changed (event)
  ((changed-class :initarg :changed-class :accessor changed-class)))
