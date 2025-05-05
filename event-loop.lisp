(in-package #:org.shirakumo.fraf.trial)

(define-global +event-pools+ (make-hash-table :test 'eq))

(defclass event ()
  ())

(defmethod clear ((event event)) event)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (event-pool (:constructor %make-event-pool (instances)))
    (instances NIL :type simple-vector)
    (index 0 :type #-ccl (unsigned-byte 32) #+ccl T)))

(defmethod print-object ((pool event-pool) stream)
  (print-unreadable-object (pool stream :type T)
    (let ((instances (event-pool-instances pool)))
      (format stream "~s ~d/~d" (type-of (aref instances 0))
              (event-pool-index pool) (length instances)))))

(defmethod clear ((event-pool event-pool))
  (setf (event-pool-index event-pool) 0))

(defun clear-event-pools ()
  (loop for pool being the hash-values of +event-pools+
        do (clear pool)))

(defun make-event-pool (class count)
  (let ((array (make-array count)))
    (dotimes (i count (%make-event-pool array))
      (setf (aref array i) (allocate-instance (ensure-class class))))))

(defun acquire-event (pool)
  (declare (optimize speed (safety 1)))
  (declare (type event-pool pool))
  (loop with starvation-counter of-type (unsigned-byte 16) = 0
        with instances of-type simple-vector = (event-pool-instances pool)
        for index of-type (unsigned-byte 32) = (event-pool-index pool)
        for next of-type (unsigned-byte 32) = (1+ index)
        do (cond ((<= next (length instances))
                  (when (atomics:cas (event-pool-index pool) index next)
                    (return (clear (aref instances index)))))
                 ((< starvation-counter 100)
                  (incf starvation-counter)
                  (sleep 0.0001))
                 (T
                  (let ((type (class-of (aref instances 0))))
                    ;; If we are starved, simply override the last instance.
                    (return (aref instances (1- (length instances)))))))))

(defun release-event (event)
  (declare (optimize speed (safety 1)))
  (declare (type event event))
  (let ((pool (gethash (type-of event) +event-pools+)))
    (etypecase pool
      (event-pool
       (loop with instances = (event-pool-instances pool)
             for index of-type (unsigned-byte 32) = (event-pool-index pool)
             for next = (max 0 (1- index))
             do (when (atomics:cas (event-pool-index pool) index next)
                  (return))))
      (null))))

(defun make-event (class &rest initargs)
  (declare (optimize speed (safety 1)))
  (declare (type symbol class))
  (let ((pool (gethash class +event-pools+)))
    (etypecase pool
      (event-pool
       (apply #'initialize-instance (acquire-event pool) initargs))
      (null
       (apply #'make-instance class initargs)))))

(define-compiler-macro make-event (&environment env class &rest initargs)
  (let ((pool (gensym "POOL")))
    `(let ((,pool ,(if (constantp class env)
                       `(load-time-value (gethash ,class +event-pools+))
                       `(gethash ,class +event-pools+))))
       (etypecase ,pool
         (event-pool
          (initialize-instance (acquire-event ,pool) ,@initargs))
         (null
          (make-instance ,class ,@initargs))))))

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
           (unwind-protect (handle event loop)
             (release-event event))))
    (declare (dynamic-extent #'handler))
    (restart-case
        (map-queue #'handler (queue loop))
      (discard-events ()
        :report "Discard all remaining events and exit"
        (discard-events loop T)))))

(defun discard-events (loop &optional (type T))
  (let ((queue (queue loop)))
    (let ((elements (queue-elements queue))
          (read (queue-read-index queue))
          (write (queue-write-index queue)))
      (loop for i from read below write
            do (when (typep (aref elements i) type)
                 (release-event (aref elements i))
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
  (form-fiddle:with-body-options (slots others pool) slots
    (when others (error "Unknown options: ~s" others))
    (let ((slots (loop for slot in slots
                       for (name default . args) = (enlist slot 'arg!)
                       collect (if (getf args :reader)
                                   (list* name default args)
                                   (list* name default :reader name args)))))
      `(progn
         (defclass ,name ,superclasses
           ,(loop for (name default . args) in slots
                  collect `(,name :initarg ,(kw name) :initform ,(if (eql default 'arg!) `(error "~a required." ',name)) ,@args)))
         ,@(when slots
             `((defmethod print-object ((event ,name) stream)
                 (print-unreadable-object (event stream :type T :identity T)
                   (format stream "~@{~a~^ ~}"
                           ,@(loop for slot in slots
                                   for reader = (getf (cddr slot) :reader)
                                   collect `(,reader event)))))
               (defmethod clear :after ((event ,name))
                 ,@(loop for (slot default) in slots
                         unless (eql default 'arg!)
                         collect `(setf (slot-value event ',slot) ,default)))))
         ,@(when pool
             `((define-event-pool ,name ,@(unless (eql pool T) (list pool)))))))))

(defmacro define-event-pool (class &optional (count 64))
  `(setf (gethash ',class +event-pools+) (make-event-pool ',class ,count)))

(define-event tick-event () tt dt fc)
(define-event pre-tick (tick-event) :pool T)
(define-event tick (tick-event) :pool T)
(define-event post-tick (tick-event) :pool T)
(define-event class-changed () changed-class)
(define-event instance-class-changed () instance)
(define-event asset-changed () changed-asset)
(define-event material-changed () changed-material)

(declaim (ftype (function (T) single-float) dt))
(declaim (ftype (function (single-float T) single-float) (setf dt)))
(declaim (ftype (function (T) double-float) tt))
(declaim (ftype (function (double-float T) double-float) (setf tt)))

(defun maybe-handle-main-event (event-type &rest initargs)
  (let ((main +main+))
    (when (and (typep main 'main) (slot-boundp main 'scene) (scene main))
      (handle (apply #'make-event event-type initargs) main))))

(define-compiler-macro maybe-handle-main-event (event-type &rest initargs)
  `(when (and (typep +main+ 'main) (slot-boundp +main+ 'scene) (scene +main+))
     (handle (make-event ,event-type ,@initargs) +main+)))
