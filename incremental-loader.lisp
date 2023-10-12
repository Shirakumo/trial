(in-package #:org.shirakumo.fraf.trial)

(defclass load-op (staging-area)
  ((observers :initform (make-hash-table :test 'eq) :accessor observers)))

(defgeneric observe-load-state (observer changing new-state load-op))
(defgeneric change-state (load-op object new-state))
(defgeneric register-load-observer (load-op observer changing))

(defmethod observe-load-state (object changing new-state (op load-op)))

(defmethod change-state ((op load-op) object new-state)
  (setf (gethash object (staged op)) new-state)
  (loop for observer in (gethash object (observers op))
        do (observe-load-state observer object new-state op)))

(defmethod register-load-observer ((op load-op) observer changing)
  (unless (member observer (gethash changing (observers op)))
    (push observer (gethash changing (observers op)))
    ;; Backfill for current state if registration occurs live.
    (let ((state (gethash changing (staged op))))
      (when state (observe-load-state observer changing state op)))))

(defmethod mark-dependent (dependency object (op load-op))
  ;; We don't care about actual staging, so just stage immediately.
  (stage dependency op))

(defmethod restage (object (op load-op))
  (setf (gethash object (staged op)) NIL)
  (stage object op))

(defmethod stage :before (object (op load-op))
  (when (eql :tentative (gethash object (staged op)))
    (error "Circular staging on ~a!" object))
  (setf (gethash object (staged op)) :tentative))

(defmethod stage :before ((object placeholder-resource) (op load-op))
  (stage (generator object) op))

(defmethod stage (object (op load-op)))

(defmethod stage ((objects cons) (op load-op))
  (dolist (object objects)
    (stage object op)))

(defmethod stage :after ((container container) (op load-op))
  (for:for ((child over container))
    (stage child op)))

(defmethod stage ((object resource) (op load-op))
  (allocate object)
  (change-state op object :allocated))

(defmethod stage ((object asset) (op load-op))
  (load object)
  (change-state op object :loaded))

(defmethod unstage ((object resource) (op load-op))
  (deallocate object)
  (change-state op object NIL))

(defmethod unstage ((object asset) (op load-op))
  (unload object)
  (change-state op object NIL))

(defmethod abort-commit ((op load-op))
  (loop for resource being the hash-keys of (staged op) using (hash-value state)
        do (case state
             ((:loaded :allocated)
              (unstage resource op)))))

(defclass incremental-loader (loader)
  ())

(defmethod commit ((op load-op) (loader incremental-loader) &key (unload T))
  (when unload
    (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
          do (case status
               ((:to-unload :to-keep :loaded)
                (unless (gethash resource (staged op))
                  (unload-with loader resource)))))
    (trivial-garbage:gc :full T))
  (let ((to-load (make-array 0 :adjustable T :fill-pointer T)))
    (loop for resource being the hash-keys of (staged op) using (hash-value status)
          do (when (typep resource 'loadable)
               (setf (gethash resource (loaded loader)) status)
               (unless (loaded-p resource)
                 (vector-push-extend resource to-load))))
    (prog1 (load-with loader to-load)
      (remhash to-load (loaded loader))
      (progress loader 100 100))))

(defmethod commit (object (loader incremental-loader) &rest args)
  (v:info :trial.loader "Incrementally loading ~a" object)
  (with-timing-report (:info :trial.loader)
    (progress loader 0 100)
    (let ((op (make-instance 'load-op)))
      (loop for resource being the hash-keys of (loaded loader) using (hash-value state)
            do (setf (gethash resource (staged op)) state))
      (stage object op)
      (apply #'commit op loader args))))

(defmethod load-with ((loader incremental-loader) (resources vector))
  (loop for resource across resources
        do (load resource)))

(defclass streamed-loader (task-runner incremental-loader)
  ((context :initform NIL :accessor context)))

(defmethod finalize :after ((loader streamed-loader))
  (finalize (context loader)))

(defmethod load-with ((loader streamed-loader) (resources vector))
  (unless (context loader)
    (setf (context loader) (create-child-context *context*)))
  (with-eval-in-task-thread (:runner loader)
    (with-context ((context loader) :reentrant T)
      (loop for resource across resources
            do (load resource)))))
