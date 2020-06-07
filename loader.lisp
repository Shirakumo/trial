#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-condition resource-depended-on (error)
  ((resource :initarg :resource)
   (dependents :initarg :dependents))
  (:report (lambda (c s) (format s "The resource~%  ~a~%cannot be unstaged as it is depended on by~{~%  ~a~}"
                                 (slot-value c 'resource) (slot-value c 'dependents)))))

(defclass staging-area ()
  ((staged :initform (make-hash-table :test 'eq) :reader staged)))

(defgeneric dependencies (object))
(defgeneric stage (object staging-area))
(defgeneric unstage (object staging-area))
(defgeneric mark-dependent (dependency object staging-area))
(defgeneric compute-load-sequence (staging-area))
(defgeneric load-before (a b))

(defmethod dependencies (object) ())

(defmethod mark-dependent (dependency object (area staging-area))
  (stage dependency area)
  ;; CAR: things this object depends on
  ;; CDR: things that depend on this object
  (pushnew dependency (car (gethash object (staged area))))
  (pushnew object (cdr (gethash dependency (staged area)))))

(defmethod load-before (a b) NIL)
(defmethod load-before ((generator resource-generator) (resource resource)) T)

(defmethod stage :around (object (area staging-area))
  (unless (gethash object (staged area))
    (call-next-method)))

(defmethod stage :after (object (area staging-area))
  (dolist (dependency (dependencies object))
    (mark-dependent dependency object)))

(defmethod stage ((resource resource) (area staging-area))
  (setf (gethash resource (staged area)) (cons NIL NIL)))

(defmethod stage ((asset asset) (area staging-area))
  (setf (gethash resource (staged area)) (cons NIL NIL)))

(defmethod stage ((container container) (area staging-area))
  (for:for ((child over container))
    (stage child area)))

(defmethod unstage ((resource resource) (area staging-area))
  (let ((data (gethash resource (staged area))))
    (when (cdr data)
      (restart-case (error 'resource-depended-on :resource resource :dependents (cdr data))
        (continue ()
          :report "Unstage the dependents too"
          (dolist (resource (cdr data))
            (unstage resource area)))
        (ignore ()
          :report "Don't unstage the resource"
          (return-from unstage))))
    ;; Remove self from dependents list
    (dolist (dependency (car data))
      (let ((data (gethash dependency (staged area))))
        (setf data (remove resource data))))
    ;; Remove entry completely
    (remhash resource (staged area))))

(defmethod compute-load-sequence ((area staging-area))
  (let ((sorted (make-array (hash-table-count (staged area)))))
    ;; First push all into the sequence, unsorted.
    (loop for object being the hash-keys of (staged area)
          for i from 0
          do (setf (aref sorted i) object))
    ;; Now sort to ensure assets and other generators come first.
    (sort sorted #'load-before)
    ;; Now perform Tarjan, which happens to be "stable-sorting".
    (let ((status (make-hash-table :test 'eq))
          (i 0))
      (labels ((visit (object)
                 (case (gethash object status :unvisited)
                   (:temporary
                    (warn "Dependency loop detected on ~a." object))
                   (:unvisited
                    (setf (gethash object status) :temporary)
                    (dolist (dependency (car (gethash object objects)))
                      (visit dependency))
                    (setf (gethash object status) :done)
                    (setf (aref sorted i) object)
                    (incf i)))))
        ;; Have to copy the sequence here to avoid overwriting as we
        ;; sort by dependencies.
        (loop for object across (copy-seq sorted)
              do (visit object))))
    sorted))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)))

(defgeneric commit (staging-area loader))
(defgeneric abort-commit (loader))
(defgeneric load-with (loader object))
(defgeneric unload-with (loader object))
(defgeneric progress (loader so-far total))

(defmethod finalize ((loader loader))
  (loop for resource being the hash-keys of (loaded loader)
        for status being the hash-values of (loaded loader)
        do (case state
             ((:to-unload :to-keep :loaded)
              (unload-with loader resource))))
  (clrhash (loaded loader)))

(defmethod abort-commit ((loader loader))
  (if (find-restart 'abort-commit)
      (invoke-restart 'abort-commit)
      (error "Not currently within a load transaction -- cannot abort.")))

(defmethod process-loads ((loader loader) loads)
  (loop with resource-states = (loaded loader)
        for i from 0 below (length loads)
        for resource = (aref loads i)
        do (case (gethash resource resource-states)
             (:to-load
              (load-with loader resource)
              (progress loader i (length loads))))))

(defmethod load-with :after ((loader loader) thing)
  (setf (gethash thing (loaded loader)) :loaded))

(defmethod unload-with :after ((loader loader) thing)
  (remhash thing (loaded loader)))

(defmethod load-with ((loader loader) (resource resource))
  (unless (allocated-p resource)
    (allocate resource)))

(defmethod load-with ((loader loader) (asset asset))
  (load asset))

(defmethod unload-with ((loader loader) (resource resource))
  (when (allocated-p resource)
    (deallocate resource)))

(defmethod unload-with ((loader loader) (asset asset))
  (unload asset))

(defmethod progress ((loader loader) so-far total))

(defmethod commit ((area staging-area) (loader loader))
  (let ((load-sequence (compute-load-sequence area))
        (resources (loaded loader)))
    ;; First, mark all resources as to-unload
    (loop for resource being the hash-keys of resources
          do (setf (gethash resource resources) :to-unload))
    ;; Next re-mark resources as keep if already loaded or to-load if new
    (loop for resource across load-sequence
          do (if (gethash resource resources)
                 (setf (gethash resource resources) :to-keep)
                 (setf (gethash resource resources) :to-load)))
    (restart-case
        (progn
          (process-loads loader load-sequence)
          ;; Now unload the ones we no longer need and reset state.
          (loop for resource being the hash-keys of resources
                for state being the hash-values of resources
                do (case state
                     (:to-unload
                      (unload-with loader resource))
                     (:to-keep
                      (setf (gethash resource resources) :loaded))))
          T)
      (abort-commit ()
        :report "Abort the commit and roll back any changes."
        ;; Unload the newly loaded resources we didn't need before, and reset the state.
        (loop for resource being the hash-keys of resources
              for state being the hash-values of resources
              do (case state
                   (:loaded
                    (unload-with loader resource))
                   (:to-load
                    (remhash resource resources))
                   ((:to-unload :to-keep)
                    (setf (gethash resource resources) :loaded))))
        NIL))))

(defmethod commit (object (loader loader))
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (commit area loader)))
