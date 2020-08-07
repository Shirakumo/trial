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
    (mark-dependent dependency object area)))

(defmethod stage (object (area staging-area)))

(defmethod stage ((object resource) (area staging-area))
  (setf (gethash object (staged area)) (cons NIL NIL)))

(defmethod stage ((object asset) (area staging-area))
  (setf (gethash object (staged area)) (cons NIL NIL)))

(defmethod stage ((container flare:container) (area staging-area))
  (for:for ((child over container))
    (stage child area)))

(defmethod stage ((entity entity) (area staging-area)))

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

(defun dependency-sort-loads (area sequence &key (status (make-hash-table :test 'eq)) (start 0) (end (length sequence)))
  (let ((objects (staged area))
        (i start))
    (labels ((visit (object)
               (case (gethash object status :invalid)
                 (:invalid
                  (setf (gethash object status) :temporary)
                  (dolist (dependency (dependencies object))
                    (visit dependency))
                  (setf (gethash object status) :validated)
                  (if (< i (length sequence))
                      (setf (aref sequence i) object)
                      (vector-push-extend object sequence))
                  (incf i))
                 (:temporary
                  (warn "Dependency loop detected on ~a." object)))))
      ;; TODO: It's possible we might be able to perform tarjan in-place
      ;;       to avoid potentially copying thousands of elements here.
      (loop for object across (subseq sequence start end)
            do (visit object))
      sequence)))

(defmethod compute-load-sequence ((area staging-area))
  (let ((sorted (make-array (hash-table-count (staged area)) :fill-pointer T :adjustable T))
        (objects (staged area)))
    ;; First push all into the sequence, unsorted.
    (loop for object being the hash-keys of objects
          for i from 0
          do (setf (aref sorted i) object))
    ;; Now sort to ensure assets and other generators come first.
    (sort sorted #'load-before)
    ;; Now perform Tarjan, which happens to be "stable-sorting".
    (dependency-sort-loads area sorted)))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)))

(defgeneric commit (staging-area loader &key unload))
(defgeneric abort-commit (loader))
(defgeneric load-with (loader object))
(defgeneric unload-with (loader object))
(defgeneric progress (loader so-far total))

(defmethod finalize ((loader loader))
  (loop for resource being the hash-keys of (loaded loader)
        for status being the hash-values of (loaded loader)
        do (case status
             ((:to-unload :to-keep :loaded)
              (unload-with loader resource))))
  (clrhash (loaded loader)))

(defmethod abort-commit ((loader loader))
  (if (find-restart 'abort-commit)
      (invoke-restart 'abort-commit)
      (error "Not currently within a load transaction -- cannot abort.")))

;; FIXME: separate things out so that a commit can be done in steps
;;        to allow the load to happen without blocking.

(defmethod process-loads ((loader loader) (area staging-area) loads)
  (let ((states (loaded loader)))
    (labels ((process-entry (i)
               (let ((resource (aref loads i)))
                 (case (gethash resource states)
                   ;; The invalid state occurs when the resource might not be
                   ;; properly sorted yet due to late dependency information.
                   (:invalid
                    ;; We /have/ to mark all future entries as :invalid first in order
                    ;; to avoid problems with entries that are marked as :validated from
                    ;; a previous re-sorting interfering with tarjan.
                    (loop for j from (1+ i) below (length loads)
                          do (setf (gethash (aref loads j) states) :invalid))
                    (dependency-sort-loads area loads :start i :status states)
                    (process-entry i))
                   ;; The validated state occurs after a late sorting has changed
                   ;; the sequence for objects that should be loaded new.
                   ((:to-load :validated)
                    (load-with loader resource)
                    (progress loader i (length loads)))))))
      ;; The load sequence can be longer after an invalid resorting,
      ;; so we need to check the length at every step.
      (loop for i from 0
            while (< i (length loads))
            do (process-entry i)))))

(defmethod load-with :after ((loader loader) thing)
  (setf (gethash thing (loaded loader)) :loaded))

(defmethod unload-with :after ((loader loader) thing)
  (remhash thing (loaded loader)))

(defmethod load-with ((loader loader) (resource resource))
  (unless (allocated-p resource)
    (allocate resource)))

(defmethod load-with ((loader loader) (asset asset))
  (unless (loaded-p asset)
    (load asset)))

(defmethod load-with :after ((loader loader) (asset asset))
  (loop with state = (loaded loader)
        for resource in (list-resources asset)
        do (setf (gethash resource state) :invalid)))

(defmethod unload-with ((loader loader) (resource resource))
  (when (allocated-p resource)
    (deallocate resource)))

(defmethod unload-with ((loader loader) (asset asset))
  (deallocate asset))

(defmethod progress ((loader loader) so-far total))

(defmethod commit ((area staging-area) (loader loader) &key (unload T))
  (let ((load-sequence (compute-load-sequence area))
        (resources (loaded loader)))
    (if unload
        ;; First, mark all resources as to-unload
        (loop for resource being the hash-keys of resources
              do (setf (gethash resource resources) :to-unload))
        (loop for resource being the hash-keys of resources
              do (setf (gethash resource resources) :to-keep)))
    ;; Next re-mark resources as keep if already loaded or to-load if new
    (loop for resource across load-sequence
          do (if (gethash resource resources)
                 (setf (gethash resource resources) :to-keep)
                 (setf (gethash resource resources) :to-load)))
    (restart-case
        (progn
          (v:info :trial.loader "About to load the following:~%  ~a" load-sequence)
          (process-loads loader area load-sequence)
          ;; Now unload the ones we no longer need and reset state.
          ;; TODO: Consider UNLOADing assets always here, since that'll just throw
          ;;       away allocation input state rather than deallocating the resources.
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

(defmethod commit (object (loader loader) &rest args)
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (apply #'commit area loader args)))
