(in-package #:org.shirakumo.fraf.trial)

(defclass staging-area ()
  ((load-state :initform (make-hash-table :test 'eq) :reader load-state)
   (observers :initform (make-hash-table :test 'eq) :reader observers)
   (loader :initarg :loader :initform NIL :accessor loader)))

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
  (let ((normalized-state (ecase new-state
                            ((:loaded :was-loaded) :loaded)
                            ((:allocated :was-allocated) :allocated)
                            ((NIL) NIL))))
    (setf (gethash object (load-state area)) new-state)
    (loop for observer across (gethash object (observers area) #())
          do (observe-load-state observer object normalized-state area))))

(defmethod register-load-observer ((area staging-area) observer changing)
  (unless (find observer (gethash changing (observers area)))
    (vector-push-extend observer (or (gethash changing (observers area))
                                     (setf (gethash changing (observers area))
                                           (make-array 0 :adjustable T :fill-pointer T))))
    ;; Backfill for current state if registration occurs live.
    (let ((state (gethash changing (load-state area))))
      (case state
        ((:loaded :was-loaded) (observe-load-state observer changing :loaded area))
        ((:allocated :was-allocated) (observe-load-state observer changing :allocated area))))))

(defmethod restage (object (area staging-area))
  (setf (gethash object (load-state area)) NIL)
  (stage object area))

(defmethod stage :around (object (area staging-area))
  (let ((state (load-state area)))
    (case (gethash object state)
      (:tentative
       (unless (typep object 'entity)
         (error "Circular staging on ~a!" object)))
      ((NIL)
       (setf (gethash object state) :tentative)
       (when (loader area)
         (let ((count (hash-table-count state)))
           (progress (loader area) count (* 1000 (1+ (truncate count 1000))))))
       (prog1 (call-next-method)
         (when (eql :tentative (gethash object state))
           (setf (gethash object state) :done)))))))

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
  (cond ((allocated-p object)
         (change-state area object :was-allocated))
        (T
         (allocate object)
         (change-state area object :allocated))))

(defmethod stage ((object asset) (area staging-area))
  (cond ((loaded-p object)
         (change-state area object :was-loaded))
        (T
         (load object)
         (change-state area object :loaded))))

(defmethod stage ((other staging-area) (area staging-area))
  (loop for resource being the hash-keys of (load-state other) using (hash-value state)
        do (setf (gethash resource (load-state area)) state)))

(defmethod unstage ((object resource) (area staging-area))
  (when (allocated-p object)
    (deallocate object))
  (change-state area object NIL))

(defmethod unstage ((object asset) (area staging-area))
  (deallocate object)
  (change-state area object NIL))

(defmethod abort-commit ((area staging-area))
  (loop for resource being the hash-keys of (load-state area) using (hash-value state)
        do (case state
             ((:loaded :allocated)
              (unstage resource area)))))

(defmethod finalize ((area staging-area))
  (abort-commit area))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)
   (current-area :initform NIL :accessor current-area)))

(defgeneric commit (staging-area loader &key unload))
(defgeneric abort-commit (loader))
(defgeneric progress (loader so-far total))

(defmethod finalize ((loader loader))
  (loop for loadable being the hash-keys of (loaded loader)
        do (typecase loadable
             (resource
              (when (allocated-p loadable)
                (deallocate loadable)))
             (asset
              (when (loaded-p loadable)
                (unload loadable))
              (deallocate loadable))))
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
                               (typecase resource
                                 (asset
                                  (when (loaded-p resource)
                                    (unload resource)))
                                 (resource
                                  (when (allocated-p resource)
                                    (deallocate resource))))
                               (remhash resource (loaded loader))))))
                 (tg:gc :full #-nx T #+nx NIL))
               ;; Persist load state into loader
               (loop for loadable being the hash-keys of (load-state area) using (hash-value status)
                     do (case status
                          ((:loaded :allocated)
                           (setf (gethash loadable (loaded loader)) status))))
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
  (if (current-area loader)
      (stage object (current-area loader))
      (with-timing-report (:info :trial.loader)
        (let ((area (make-instance 'staging-area :loader loader)))
          (with-cleanup-on-failure (abort-commit area)
            (stage object area))
          (apply #'commit area loader args)))))

(defmethod abort-commit ((loader loader))
  (cond ((find-restart 'abort-commit)
         (invoke-restart 'abort-commit))
        ((current-area loader)
         (abort-commit (current-area loader))
         (setf (current-area loader) NIL))))

(defmethod load-with ((loader loader) (resources vector))
  (loop for resource across resources
        for i from 0
        do (load resource)
           (progress loader i (length resources))))

(defmethod commit ((area staging-area) (loader (eql T)) &rest args)
  (apply #'commit area (loader area) args))

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
