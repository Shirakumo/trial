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

#-elide-handler-restarts
(defmethod handle :around ((event event) listener)
  (restart-case
      (call-next-method)
    (abort ()
      :report (lambda (s) (format s "Don't handle ~a in ~a." event listener))
      NIL)
    (leave ()
      :report (lambda (s) (format s "Leave ~a from the loop." listener))
      (leave listener T))))

;; Default to doing nothing.
(defmethod handle ((event event) (listener listener)))

(defclass event-loop ()
  ((queue :initform (make-queue) :reader queue)
   (listeners :initform (make-hash-table :test 'eq) :accessor listeners)
   (listener-queue :initform '(NIL) :accessor listener-queue)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-instance event-type args)))))
    (queue-push event (queue loop))))

(define-compiler-macro issue (&environment env loop event-type &rest args)
  (cond ((and (constantp event-type env)
              (listp event-type)
              (eql (first event-type) 'quote)
              (symbolp (second event-type)))
         `(queue-push (make-instance ,event-type ,@args) (queue ,loop)))
        (T
         (let ((eventg (gensym "EVENT")))
           `(let* ((,eventg ,event-type)
                   (,eventg (etypecase ,eventg
                              (event ,eventg)
                              ((or class symbol)
                               (make-instance ,eventg ,@args)))))
              (queue-push ,eventg (queue ,loop)))))))

(defmethod process ((loop event-loop))
  (declare (optimize speed))
  (flet ((handler (event)
           (handle event loop)))
    (declare (dynamic-extent #'handler))
    (restart-case
        (map-queue #'handler (queue loop))
      (discard-events ()
        :report "Discard all remaining events and exit"
        (queue-discard (queue loop))))))

(defun discard-events (loop &optional (type T))
  (let ((queue (queue loop)))
    (loop for i from 0 below (length queue)
          do (when (typep (aref queue i) type)
               (setf (aref queue i) NIL)))))

(defmethod handle ((event event) (loop event-loop))
  (with-simple-restart (skip-event "Skip handling the event entirely.")
    (loop with queue = (listener-queue loop)
          for listener = (pop queue)
          while listener
          do (handle event listener))))

(defmethod handle ((event event) (fun function))
  (funcall fun event))

;; FIXME: make this thread safe
;; NOTE: we have the LISTENER-QUEUE in order to ensure we can remove arbitrary
;;       listeners //during// event handling, which we could not do if we iterated
;;       the hash table directly
(defmethod add-listener (listener (loop event-loop))
  (if (gethash listener (listeners loop))
      listener
      (let ((cons (cons listener (listener-queue loop))))
        (setf (gethash listener (listeners loop)) cons)
        (setf (listener-queue loop) cons)
        listener)))

(defmethod remove-listener (listener (loop event-loop))
  (let* ((listeners (listeners loop))
         (cons (gethash listener listeners)))
    (declare (type hash-table listeners))
    (when cons
      (setf (car cons) (cadr cons))
      (setf (cdr cons) (cddr cons))
      (setf (gethash (car cons) listeners) cons))
    (remhash listener listeners)
    listener))

(defmethod clear ((loop event-loop))
  (discard-events loop)
  (clrhash (listeners loop))
  (setf (listener-queue loop) '(NIL)))

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
