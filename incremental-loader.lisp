(in-package #:org.shirakumo.fraf.trial)

(defclass load-op (staging-area)
  ((observers :initform (make-hash-table :test 'eq) :accessor observers)))

(defgeneric observe-load-state (load-op observer changing new-state))
(defgeneric change-state (load-op object new-state))
(defgeneric register-load-observer (load-op observer changing))

(defmethod change-state ((op load-op) object new-state)
  (setf (gethash object (staged op)) new-state)
  (loop for observer in (gethash object (observers op))
        do (observe-load-state op observer object new-state)))

(defmethod register-load-observer ((op load-op) observer changing)
  (unless (member observer (gethash changing (observers op)))
    (push observer (gethash changing (observers op)))
    ;; Backfill for current state if registration occurs live.
    (let ((state (gethash changing (staged op))))
      (when state (observe-load-state op observer changing state)))))

(defmethod stage :around (object (op load-op))
  (unless (gethash object (staged op))
    (call-next-method)))

(defmethod stage :before (object (op load-op))
  (dolist (dependency (dependencies object))
    (stage dependency op)))

(defmethod stage (object (op load-op)))

(defmethod stage ((objects cons) (op load-op))
  (dolist (object objects)
    (stage object op)))

(defmethod stage :after ((container container) (op load-op))
  (for:for ((child over container))
           (stage child op)))

(defmethod stage ((object resource) (op load-op))
  (change-state op object :to-allocate)
  (allocate object)
  (change-state op object :allocated))

(defmethod stage ((object asset) (op load-op))
  (change-state op object :to-load)
  (load object)
  (change-state op object :loaded))

(defmethod unstage ((object resource) (op load-op))
  (deallocate asset)
  (change-state op object NIL))

(defmethod unstage ((object asset) (op load-op))
  (unload asset)
  (change-state op object NIL))

(defmethod abort-commit ((op load-op))
  (loop for resource being the hash-keys of (staged op) using (hash-value state)
        do (case state
             ((:loaded :allocated)
              (unstage resource op)))))

(defclass incremental-loader (loader)
  ())

(defmethod commit ((op load-op) (loader incremenal-loader) &key (unload T))
  (when unload
    (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
          do (case status
               ((:to-unload :to-keep :loaded)
                (unless (gethash resource (staged op))
                  (unload-with loader resource))))))
  (let ((to-load (make-array 0 :adjustable T :fill-pointer T)))
    (loop for resource being the hash-keys of (staged op) using (hash-value status)
          do (setf (gethash resource (loaded loader)) status)
             (unless (loaded-p resource)
               (vector-push-extend resource to-load)))
    (load-with loader to-load)))

(defmethod load-with ((loader incremental-loader) (resources vector))
  (loop for resource across resources
        do (load resource)))

(defclass streamed-loader (task-runner incremental-loader)
  ((context :initform NIL :accessor context)))

(defmethod finalize :after ((loader streamed-loader))
  (finalize (context loader)))

(defmethod load-with ((loader incremental-loader) (resources vector))
  (unless (context loader)
    (setf (context loader) (create-child-context *context*)))
  (with-eval-in-task-thread (:runner loader)
    (with-context ((context loader) :reentrant T)
      (loop for resource across resources
            do (load resource)))))
