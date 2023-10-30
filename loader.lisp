(in-package #:org.shirakumo.fraf.trial)

(defclass staging-area ()
  ((load-state :initform (make-hash-table :test 'eq) :reader load-state)
   (observers :initform (make-hash-table :test 'eq) :reader observers)))

(defgeneric dependencies (object))
(defgeneric stage (object staging-area))
(defgeneric unstage (object staging-area))
(defgeneric observe-load-state (observer changing new-state staging-area))
(defgeneric change-state (staging-area object new-state))
(defgeneric register-load-observer (staging-area observer changing))

(defmethod dependencies (object) ())
(defmethod stage (object (area staging-area)))
(defmethod observe-load-state (object changing new-state (area staging-area)))

(defmethod change-state ((area staging-area) object new-state)
  (setf (gethash object (load-state area)) new-state)
  (loop for observer in (gethash object (observers area))
        do (observe-load-state observer object new-state area)))

(defmethod register-load-observer ((area staging-area) observer changing)
  (unless (member observer (gethash changing (observers area)))
    (push observer (gethash changing (observers area)))
    ;; Backfill for current state if registration occurs live.
    (let ((state (gethash changing (load-state area))))
      (when state (observe-load-state observer changing state area)))))

(defmethod restage (object (area staging-area))
  (setf (gethash object (load-state area)) NIL)
  (stage object area))

(defmethod stage :around (object (area staging-area))
  (case (gethash object (load-state area))
    (:tentative
     (error "Circular staging on ~a!" object))
    ((NIL)
     (setf (gethash object (load-state area)) :tentative)
     (prog1 (call-next-method)
       (when (eql :tentative (gethash object (load-state area)))
         (setf (gethash object (load-state area)) :done))))))

(defmethod stage :before (object (area staging-area))
  (dolist (dependency (dependencies object))
    (stage dependency area)))

(defmethod stage ((objects cons) (area staging-area))
  (dolist (object objects)
    (stage object area)))

(defmethod stage :after ((container container) (area staging-area))
  (for:for ((child over container))
    (stage child area)))

(defmethod stage :before ((object resource) (area staging-area))
  (when (generator object)
    (stage (generator object) area)))

(defmethod stage ((object resource) (area staging-area))
  (unless (allocated-p object)
    (allocate object))
  (change-state area object :allocated))

(defmethod stage ((object asset) (area staging-area))
  (unless (loaded-p object)
    (load object))
  (change-state area object :loaded))

(defmethod stage ((other staging-area) (area staging-area))
  (loop for resource being the hash-keys of (load-state other) using (hash-value state)
        do (setf (gethash resource (load-state area)) state)))

(defmethod unstage ((object resource) (area staging-area))
  (deallocate object)
  (change-state area object NIL))

(defmethod unstage ((object asset) (area staging-area))
  (unload object)
  (change-state area object NIL))

(defmethod abort-commit ((area staging-area))
  (loop for resource being the hash-keys of (load-state area) using (hash-value state)
        do (case state
             ((:loaded :allocated)
              (unstage resource area)))))

(defmethod deallocate ((area staging-area))
  (abort-commit area))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)
   (current-area :initform NIL :accessor current-area)))

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

(defun dependency-sort-loads (sequence &key (status (make-hash-table :test 'eq)))
  (let ((orig-seq (copy-seq sequence)))
    (labels ((visit (object)
               (case (gethash object status :invalid)
                 (:invalid
                  (setf (gethash object status) :temporary)
                  (dolist (dependency (dependencies object))
                    (when dependency
                      (visit dependency)))
                  (setf (gethash object status) :validated)
                  (vector-push-extend object sequence))
                 (:temporary
                  (warn "Dependency loop detected on ~a." object)))))
      ;; TODO: It's possible we might be able to perform tarjan in-place
      ;;       to avoid potentially copying thousands of elements here.
      (setf (fill-pointer sequence) 0)
      (map NIL #'visit orig-seq)
      sequence)))

(defmethod commit ((area staging-area) (loader loader) &key (unload T))
  (cond ((eq area (current-area loader)))
        ((current-area loader)
         (stage area (current-area loader)))
        (T
         (with-unwind-protection (setf (current-area loader) NIL)
           (setf (current-area loader) area)
           (with-simple-restart (abort-commit "Abort the load operation")
             (with-cleanup-on-failure (abort-commit area)
               (when unload
                 (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
                       do (case status
                            ((:loaded :allocated)
                             (unless (gethash resource (load-state area))
                               (deallocate resource)
                               (remhash resource (loaded loader))))))
                 (trivial-garbage:gc :full T))
               (let ((to-load (make-array 0 :adjustable T :fill-pointer T)))
                 (loop for resource being the hash-keys of (load-state area) using (hash-value status)
                       do (when (typep resource 'loadable)
                            (setf (gethash resource (loaded loader)) status)
                            (unless (loaded-p resource)
                              (vector-push-extend resource to-load))))
                 (prog1 (load-with loader (dependency-sort-loads to-load))
                   (remhash to-load (loaded loader))
                   (progress loader 100 100)))))))))

(defmethod commit (object (loader loader) &rest args)
  (v:info :trial.loader "Incrementally loading ~a" object)
  (with-timing-report (:info :trial.loader)
    (progress loader 0 100)
    (if (current-area loader)
        (stage object (current-area loader))
        (let ((area (make-instance 'staging-area)))
          (stage object area)
          (apply #'commit area loader args)))))

(defmethod abort-commit ((loader loader))
  (cond ((find-restart 'abort-commit)
         (invoke-restart 'abort-commit))
        ((current-area loader)
         (abort-commit (current-area loader))
         (setf (current-area loader) NIL))))

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

(defmethod abort-commit ((loader streamed-loader))
  ;; TODO: implement commit abort for streamed loaders
  (implement!))
