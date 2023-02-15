#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass task-thread (simple-tasks:queued-runner)
  ((thread :initform NIL :accessor thread)))

(defmethod start ((runner task-thread))
  (unless (and (thread runner)
               (bt:thread-alive-p (thread runner)))
    (setf (thread runner)
          (with-thread ("task-thread")
            (simple-tasks:start-runner runner)))
    (loop until (eql :running (simple-tasks:status runner))
          do (sleep 0.0001))))

(defmethod stop ((runner task-thread))
  (handler-case (simple-tasks:stop-runner runner)
    (simple-tasks:runner-not-stopped ()
      (bt:destroy-thread (thread runner)))))

(defmethod simple-tasks:schedule-task ((function function) (main task-thread))
  (simple-tasks:schedule-task
   (make-instance 'promise-task :promise (promise:make) :func function)
   main))

(defclass task-runner-main (main)
  ((task-thread :initform (make-instance 'task-thread) :accessor task-thread)))

(defmethod initialize-instance :after ((main task-runner-main) &key)
  (start (task-thread main)))

(defmethod finalize :after ((main task-runner-main))
  (stop (task-thread main)))

(defmethod update :before ((main task-runner-main) tt dt fc)
  (promise:tick-all dt))

(defmethod simple-tasks:schedule-task (task (default (eql T)))
  (simple-tasks:schedule-task task +main+))

(defmethod simple-tasks:schedule-task (task (main task-runner-main))
  (simple-tasks:schedule-task task (task-thread +main+)))

(defclass promise-task (simple-tasks:task)
  ((promise :initarg :promise :accessor promise)
   (func :initarg :func :accessor func)))

(defmethod simple-tasks:run-task ((task promise-task))
  (handler-case (promise:succeed (promise task) (funcall (func task)))
    (error (e) (promise:fail (promise task) e))))

(defmacro with-eval-in-task-thread ((&key (runner '(task-thread +main+)) (task-type ''promise-task) lifetime) &body body)
  `(flet ((thunk () ,@body))
     (simple-tasks:schedule-task
      (make-instance ,task-type :promise (promise:make NIL :lifetime ,lifetime)
                                :func #'thunk)
      ,runner)))
