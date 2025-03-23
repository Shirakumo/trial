(in-package #:org.shirakumo.fraf.trial)

(defclass task-thread (simple-tasks:queued-runner)
  ((thread :initform NIL :accessor thread)))

(defmethod start ((runner task-thread))
  (unless (and (thread runner)
               (bt:thread-alive-p (thread runner)))
    (setf (thread runner)
          (with-thread ("task-thread")
            #+nx (setf (org.shirakumo.machine-state:thread-core-mask T) #b0100)
            (simple-tasks:start-runner runner)))
    (loop until (eql :running (simple-tasks:status runner))
          do (sleep 0.0001))))

(defmethod stop ((runner task-thread))
  (handler-case (simple-tasks:stop-runner runner)
    (simple-tasks:runner-not-stopped ()
      (bt:destroy-thread (thread runner)))))

(defmethod simple-tasks:start-runner ((runner task-thread))
  (handler-bind (#+trial-release (error (lambda (e)
                                          (v:warn :trial.async "Ignoring failure in task thread: ~a" e)
                                          (v:debug :trial.async e)
                                          (invoke-restart 'simple-tasks:skip))))
    (call-next-method)))

(defmethod simple-tasks:schedule-task ((function function) (main task-thread))
  (simple-tasks:schedule-task
   (make-instance 'promise-task :promise (promise:make) :func function)
   main))

(defclass task-runner ()
  ((task-thread :initform (make-instance 'task-thread) :accessor task-thread)))

(defmethod initialize-instance ((runner task-runner) &key)
  (call-next-method)
  (start (task-thread runner)))

(defmethod finalize :after ((runner task-runner))
  (stop (task-thread runner)))

(defmethod simple-tasks:schedule-task (task (runner task-runner))
  (simple-tasks:schedule-task task (task-thread runner)))

(defclass task-runner-main (task-runner main)
  ())

(defmethod update :before ((main task-runner-main) tt dt fc)
  (handler-bind (#+trial-release (error #'abort))
    (promise:tick-all dt)))

(defmethod simple-tasks:schedule-task (task (default (eql T)))
  (simple-tasks:schedule-task task +main+))

(defclass promise-task (simple-tasks:task)
  ((promise :initarg :promise :accessor promise :reader promise:ensure-promise)
   (func :initarg :func :accessor func)))

(defmethod simple-tasks:run-task ((task promise-task))
  (let ((ok NIL))
    (unwind-protect
         (restart-case
             (progn (promise:succeed (promise task) (funcall (func task)))
                    (setf ok T))
           (use-value (value)
             :report "Succeed the promise using the provided value"
             (promise:succeed (promise task) value)
             (setf ok T))
           (continue ()
             :report "Continue, failing the promise.")
           (abort ()
             :report "Abort, timing the promise out."
             (promise:timeout (promise task))))
      (unless ok
        (promise:fail (promise task))))))

(defmacro with-eval-in-task-thread ((&key (runner '(task-thread +main+)) (task-type ''promise-task) lifetime) &body body)
  `(flet ((thunk () ,@body))
     (simple-tasks:schedule-task
      (make-instance ,task-type :promise (promise:make NIL :lifetime ,lifetime)
                                :func #'thunk)
      ,runner)))
