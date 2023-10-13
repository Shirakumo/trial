(in-package #:org.shirakumo.fraf.trial)

(defclass staging-area ()
  ((staged :initform (make-hash-table :test 'eq) :reader staged)
   (observers :initform (make-hash-table :test 'eq) :accessor observers)))

(defgeneric dependencies (object))
(defgeneric stage (object staging-area))
(defgeneric unstage (object staging-area))
(defgeneric observe-load-state (observer changing new-state staging-area))
(defgeneric change-state (staging-area object new-state))
(defgeneric register-load-observer (staging-area observer changing))

(defmethod dependencies (object) ())

(defmethod observe-load-state (object changing new-state (area staging-area)))

(defmethod change-state ((area staging-area) object new-state)
  (setf (gethash object (staged area)) new-state)
  (loop for observer in (gethash object (observers area))
        do (observe-load-state observer object new-state area)))

(defmethod register-load-observer ((area staging-area) observer changing)
  (unless (member observer (gethash changing (observers area)))
    (push observer (gethash changing (observers area)))
    ;; Backfill for current state if registration occurs live.
    (let ((state (gethash changing (staged area))))
      (when state (observe-load-state observer changing state area)))))

(defmethod restage (object (area staging-area))
  (setf (gethash object (staged area)) NIL)
  (stage object area))

(defmethod stage (object (area staging-area)))

(defmethod stage :around (object (area staging-area))
  (case (gethash object (staged area))
    (:tentative
     (error "Circular staging on ~a!" object))
    ((NIL)
     (setf (gethash object (staged area)) :tentative)
     (prog1 (call-next-method)
       (when (eql :tentative (gethash object (staged area)))
         (setf (gethash object (staged area)) :done))))))

(defmethod stage :before (object (area staging-area))
  (dolist (dependency (dependencies object))
    (stage dependency area)))

(defmethod stage ((objects cons) (area staging-area))
  (dolist (object objects)
    (stage object area)))

(defmethod stage :after ((container container) (area staging-area))
  (for:for ((child over container))
    (stage child area)))

(defmethod stage :before ((object placeholder-resource) (area staging-area))
  (stage (generator object) area))

(defmethod stage ((object resource) (area staging-area))
  (allocate object)
  (change-state area object :allocated))

(defmethod stage ((object asset) (area staging-area))
  (load object)
  (change-state area object :loaded))

(defmethod unstage ((object resource) (area staging-area))
  (deallocate object)
  (change-state area object NIL))

(defmethod unstage ((object asset) (area staging-area))
  (unload object)
  (change-state area object NIL))

(defmethod abort-commit ((area staging-area))
  (loop for resource being the hash-keys of (staged area) using (hash-value state)
        do (case state
             ((:loaded :allocated)
              (unstage resource area)))))

(defmethod deallocate ((area staging-area))
  (abort-commit area))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)))

(defgeneric commit (staging-area loader &key unload))
(defgeneric abort-commit (loader))
(defgeneric progress (loader so-far total))

(defmethod finalize ((loader loader))
  (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
        do (case status
             ((:loaded :allocated)
              (when (loaded-p resource)
                (deallocate resource)))))
  (clrhash (loaded loader)))

(defmethod progress ((loader loader) so-far total))

(defmethod commit ((area staging-area) (loader loader) &key (unload T))
  (when unload
    (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
          do (case status
               ((:loaded :allocated)
                (unless (gethash resource (staged area))
                  (deallocate resource)
                  (remhash resource (loaded loader))))))
    (trivial-garbage:gc :full T))
  (let ((to-load (make-array 0 :adjustable T :fill-pointer T)))
    (loop for resource being the hash-keys of (staged area) using (hash-value status)
          do (when (typep resource 'loadable)
               (setf (gethash resource (loaded loader)) status)
               (unless (loaded-p resource)
                 (vector-push-extend resource to-load))))
    (prog1 (load-with loader to-load)
      (remhash to-load (loaded loader))
      (progress loader 100 100))))

(defmethod commit (object (loader loader) &rest args)
  (v:info :trial.loader "Incrementally loading ~a" object)
  (with-timing-report (:info :trial.loader)
    (progress loader 0 100)
    (let ((area (make-instance 'staging-area)))
      (with-cleanup-on-failure (abort-commit area)
        (loop for resource being the hash-keys of (loaded loader) using (hash-value state)
              do (setf (gethash resource (staged area)) state))
        (stage object area)
        (apply #'commit area loader args)))))

(defmethod load-with ((loader loader) (resources vector))
  (loop for resource across resources
        do (load resource)))

(defclass streamed-loader (task-runner loader)
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
