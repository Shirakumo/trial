(in-package #:org.shirakumo.fraf.trial)

(define-global +event-pools+ (make-hash-table :test 'eq))

(defclass event ()
  ())

(defclass listener ()
  ())

(defstruct (event-pool (:constructor %make-event-pool (instances)))
  (instances NIL :type simple-vector)
  (index 0 :type (unsigned-byte 32)))

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

(defun make-event-pool (class count)
  (let ((array (make-array count)))
    (dotimes (i count (%make-event-pool array))
      (setf (aref array i) (allocate-instance (ensure-class class))))))

(defun make-event (class &rest initargs)
  (let ((pool (gethash class +event-pools+)))
    (if pool
        (loop
         (let* ((index (event-pool-index pool))
                (instances (event-pool-instances pool)))
           (when (atomics:cas (event-pool-index pool) index (mod (1+ index) (length instances)))
             (return (apply #'initialize-instance (aref instances index) initargs)))))
        (apply #'make-instance class initargs))))

(define-compiler-macro make-event (&environment env class &rest initargs)
  (let ((pool (gensym "POOL"))
        (index (gensym "INDEX"))
        (instances (gensym "INSTANCES")))
    `(let ((,pool ,(if (constantp class env)
                       `(load-time-value (gethash ,class +event-pools+))
                       `(gethash ,class +event-pools+))))
       (if ,pool
           (loop
            (let* ((,index (event-pool-index ,pool))
                   (,instances (event-pool-instances ,pool)))
              (when (atomics:cas (event-pool-index ,pool) ,index (mod (1+ ,index) (length ,instances)))
                (return (initialize-instance (aref ,instances ,index) ,@initargs)))))
           (make-instance ,class ,@initargs)))))

(defclass event-loop ()
  ((queue :initform (make-queue) :reader queue)
   (listeners :initform (make-hash-table :test 'eq) :accessor listeners)
   (listener-queue :initform '(NIL) :accessor listener-queue)))

(defun issue (loop event-type &rest args)
  (let ((event (etypecase event-type
                 (event event-type)
                 ((or class symbol)
                  (apply #'make-event event-type args))))
        (loop (etypecase loop
                (event-loop loop)
                ((eql T) (scene +main+)))))
    (queue-push event (queue loop))))

(define-compiler-macro issue (&environment env loop event-type &rest args)
  (let ((event (cond ((and (constantp event-type env)
                           (listp event-type)
                           (eql (first event-type) 'quote)
                           (symbolp (second event-type)))
                      `(make-event ,event-type ,@args))
                     (T
                      (let ((eventg (gensym "EVENT")))
                        `(let ((,eventg ,event-type))
                           (etypecase ,eventg
                             (event ,eventg)
                             ((or class symbol)
                              (make-event ,eventg ,@args))))))))
        (loop (cond ((eql T loop)
                     `(scene +main+))
                    (T
                     (let ((loopg (gensym "LOOP")))
                       `(let ((,loopg ,loop))
                          (etypecase ,loopg
                            (event-loop ,loopg)
                            ((eql T) (scene +main+)))))))))
    `(queue-push ,event (queue ,loop))))

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
    (let ((elements (queue-elements queue))
          (read (queue-read-index queue))
          (write (queue-write-index queue)))
      (loop for i from read below write
            do (when (typep (aref elements i) type)
                 (setf (aref elements i) NIL)))
      queue)))

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
         (block NIL
           (let ,(loop for slot in slots
                       for (var name) = (enlist slot slot)
                       collect `(,var (slot-value ,variable ',name)))
             ,@body))))))

(defmacro undefine-handler ((class event &rest qualifiers) slots &body body)
  (declare (ignore slots body))
  (destructuring-bind (instance class) (enlist class class)
    (destructuring-bind (variable event) (enlist event event)
      `(undefmethod handle ,@qualifiers ((,variable ,event) (,instance ,class))))))

(defmacro define-event (name superclasses &body slots)
  (unless (find 'event superclasses)
    (setf superclasses (append superclasses '(event))))
  `(progn
     (defclass ,name ,superclasses
       ,(loop for slot in slots
              collect (destructuring-bind (name &optional (default 'arg!) &rest args) (enlist slot)
                        (unless (getf args :reader)
                          (setf (getf args :reader) name))
                        `(,name :initarg ,(kw name) :initform ,(if (eql default 'arg!) `(error "~a required." ',name)) ,@args))))

     (defmethod print-object ((event ,name) stream)
       (print-unreadable-object (event stream :type T :identity T)
         (format stream "~@{~a~^ ~}"
                 ,@(loop for slot in slots
                         collect `(,(first (enlist slot)) event)))))))

(defmacro define-event-pool (class &optional (count 32))
  `(setf (gethash ',class +event-pools+) (make-event-pool ',class ,count)))

(define-event tick-event () tt dt fc)
(define-event pre-tick (tick-event))
(define-event tick (tick-event))
(define-event post-tick (tick-event))
(define-event class-changed () changed-class)
(define-event instance-class-changed () instance)
(define-event asset-changed () changed-asset)

(define-event-pool pre-tick)
(define-event-pool tick)
(define-event-pool post-tick)
