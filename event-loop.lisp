#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
    (setf (handlers container) (sort (nconc handlers (handlers container)) #'> :key #'priority)))
  handlers)

;; Default priority
(defmethod priority (object)
  0)

(defmethod add-handler ((source handler-container) (container handler-container))
  (add-handler (handlers source) container)
  source)

(defmethod remove-handler (handler (container handler-container))
  (setf (handlers container)
        (delete handler (handlers container) :test #'matches))
  handler)

(defmethod remove-handler ((handlers list) (container handler-container))
  (setf (handlers container) (delete-if (lambda (el)
                                          (find el handlers :test #'matches))
                                        (handlers container)))
  handlers)

(defmethod remove-handler ((source handler-container) (container handler-container))
  (remove-handler (handlers source) container))

(defmethod handle (event (container handler-container))
  (dolist (handler (handlers container))
    (handle event handler)))

(defmethod handle :around (event handler)
  (with-simple-restart (abort "Don't handle ~a in ~a." event handler)
    (call-next-method)))

(defclass event-loop (handler-container)
  ((queue :initform (make-array 64 :initial-element NIL :adjustable T :fill-pointer 0) :reader queue)
   (queue-index :initform 0 :accessor queue-index)))

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
  (loop for i = (1- (incf (queue-index loop)))
        while (< i (length (queue loop)))
        do (let ((event (aref (queue loop) i)))
             (when event
               (handle event loop)
               (setf (aref (queue loop) i) NIL))))
  (setf (fill-pointer (queue loop)) 0
        (queue-index loop) 0))

(defun discard-events (loop)
  (loop for i = (1- (incf (queue-index loop)))
        while (< i (length (queue loop)))
        do (setf (aref (queue loop) i) NIL))
  (setf (fill-pointer (queue loop)) 0
        (queue-index loop) 0))

(defmethod handle (event (loop event-loop))
  (with-simple-restart (skip-event "Skip handling the event entirely.")
    (call-next-method)))

;; Force adding the loop directly.
(defmethod add-handler ((loop event-loop) (container handler-container))
  (setf (handlers container) (cons loop (delete loop (handlers container) :test #'matches)))
  loop)

(defmethod remove-handler ((loop event-loop) (container handler-container))
  (setf (handlers container) (delete loop (handlers container) :test #'matches))
  loop)

(defclass handler ()
  ((name :initarg :name :accessor name)
   (event-type :initarg :event-type :accessor event-type)
   (delivery-function :initarg :delivery-function :accessor delivery-function)
   (priority :initarg :priority :accessor priority))
  (:default-initargs
   :name (error "NAME needed.")
   :event-type 'event
   :delivery-function (error "DELIVERY-FUNCTION needed.")
   :priority 0))

(defmethod matches ((a handler) (b handler))
  (eql (name a) (name b)))

(defmethod handle :around (event (handler handler))
  (when (typep event (event-type handler))
    (call-next-method)))

(defmethod handle (event (handler handler))
  (funcall (delivery-function handler) event))

(defclass event ()
  ())

(defclass tick (event)
  ((tt :initarg :tt :accessor tt)
   (dt :initarg :dt :accessor dt)))

(defclass class-changed (event)
  ((changed-class :initarg :changed-class :accessor changed-class)))
