#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;; FIXME: How do we access extra information that is not the straight-up content?
;;        stuff like dimensions of a texture or mesh material or whatever?
;;        How do we "bind" the asset if it's needed? Spilling the CONTENT everywhere
;;        seems like a bad idea.
;; FIXME: Resources need to be sectioned off on a per-context basis so that clashing
;;        cannot occur. How to generalise without losing performance?

(defvar *redefining* NIL)
(defvar *pools* (make-hash-table :test 'eql))
(defvar *standalone* NIL)

;; forward definition
(defclass asset () ())
(defclass pool () ())

(defmethod pool (name)
  (gethash (intern (string-upcase name) :KEYWORD) *pools*))

(defmethod pool ((name symbol))
  (if (keywordp name)
      (gethash name *pools*)
      (pool (string name))))

(defmethod (setf pool) ((pool pool) name)
  (setf (gethash (intern (string-upcase name) :KEYWORD) *pools*) pool))

(defmethod (setf pool) ((pool pool) (name symbol))
  (if (keywordp name)
      (setf (gethash name *pools*) pool)
      (setf (pool (string name)) pool)))

(defun remove-pool (name)
  (remhash name *pools*))

(defun pools ()
  (alexandria:hash-table-values *pools*))

(defun resolve-pool-base (base)
  (pathname-utils:normalize-pathname
   (if *standalone*
       (let ((root (merge-pathnames "pool/" (executable-directory))))
         (etypecase base
           (string (merge-pathnames (uiop:parse-native-namestring base) root))
           (pathname (merge-pathnames base root))
           (symbol (merge-pathnames (format NIL "~(~a~)/" base) root))
           (list (merge-pathnames (merge-pathnames (second base) (format NIL "~(~a~)/" (first base))) root))))
       (etypecase base
         (string (uiop:parse-native-namestring base))
         (pathname base)
         (symbol (asdf:system-relative-pathname base "data/"))
         (list (asdf:system-relative-pathname (first base) (second base)))))))

(defun reconfigure-pool-bases ()
  (mapc #'reinitialize-instance (pools)))

(defun prepare-standalone ()
  (setf *standalone* T)
  (reconfigure-pool-bases))

(pushnew 'prepare-standalone qtools:*boot-hooks*)

(defclass pool ()
  ((name :initarg :name  :initform NIL :reader name)
   (base-designator :accessor base-designator)
   (base :initform NIL :accessor base)
   (assets :initform () :accessor assets))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod print-object ((pool pool) stream)
  (print-unreadable-object (pool stream :type T)
    (format stream "~s ~s" (name pool) (base pool))))

(defmethod initialize-instance :after ((pool pool) &key base)
  (let ((prev (pool (name pool))))
    (when prev
      (setf (assets pool) (assets prev))))
  (setf (pool (name pool)) pool)
  (setf (base pool) (or base (name pool))))

(defmethod reinitialize-instance :after ((pool pool) &key (base (base-designator pool)))
  (setf (base pool) base))

(defmethod (setf base) (base (pool pool))
  (setf (base-designator pool) base)
  (let ((path (resolve-pool-base base)))
    (unless (uiop:directory-exists-p path)
      (warn "The pool base ~&  ~s~&resolved from ~s does not exist."
            path base))
    (setf (slot-value pool 'base) path)
    ;; Update all assets that are local to this.
    (dolist (asset (assets pool))
      (when (eql pool (home asset))
        (reinitialize-instance asset)))))

(defmethod pool ((pool pool))
  pool)

(defmethod enter ((asset asset) (name symbol))
  (enter asset (pool name)))

(defmethod enter ((asset asset) (pool pool))
  (leave asset pool)
  (push asset (assets pool))
  asset)

(defmethod leave ((asset asset) (name symbol))
  (leave asset (pool name)))

(defmethod leave ((asset asset) (pool pool))
  (setf (assets pool) (remove asset (assets pool) :test #'matches))
  asset)

(defmethod asset (type (pool symbol) name)
  (asset type
         (or (pool pool)
             (error "No such pool ~a" pool))
         name))

;; FIXME: Maybe optimise asset access if it turns out to be slow.
(defmethod asset (type (pool pool) name)
  (find-if (lambda (asset) (and (eql (type-of asset) type)
                                (string-equal (name asset) name)))
           (assets pool)))

(defmethod restore ((pool pool))
  (mapc #'restore (assets pool)))

(defmethod offload ((pool pool))
  (mapc #'offload (assets pool)))

(defmacro define-pool (name &body options)
  `(name (make-instance 'pool :name ',name ,@options)))

(defclass asset ()
  ((name :initarg :name :initform NIL :accessor name)
   (home :initform NIL :accessor home)
   (resource :initform NIL :accessor resource)
   (dependencies :initarg :dependencies :initarg :depends-on :initform NIL :accessor dependencies))
  (:default-initargs
   :name (error "NAME required.")
   :home (error "HOME required.")
   :dependencies NIL))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~a::~a~@[ LOADED~]"
            (when (home asset) (string-upcase (name (home asset))))
            (name asset) (resource asset))))

;; We need to do this in two phases because we need to ensure that the home slot
;; is ready during init, but still give the asset a chance to do things before
;; we are actually entered into the home.
(defmethod shared-initialize :after ((asset asset) slot-names &key home)
  (when home
    (when (home asset)
      (leave asset (home asset)))
    (setf (home asset) home)))

(defmethod initialize-instance :after ((asset asset) &key home)
  (when home
    (enter asset home)))

(defmethod reinitialize-instance :around ((asset asset) &key home)
  (let ((*redefining* T))
    (call-next-method))
  (when home
    (enter asset home))
  (reload asset))

(defmethod (setf home) (pool (asset asset))
  (setf (slot-value asset 'home) (pool pool)))

(defmethod loaded-p ((asset asset))
  (and (resource asset) (slot-value (resource asset) 'data)))

(defmethod resource ((asset asset))
  (let ((maybe-pointer (slot-value asset 'resource)))
    (when maybe-pointer (tg:weak-pointer-value maybe-pointer))))

(defmethod (setf resource) (value (asset asset))
  (setf (slot-value asset 'resource) (when value (tg:make-weak-pointer value)))
  value)

(defmethod data ((asset asset))
  (let ((resource (resource asset)))
    (and resource (data resource))))

(defmethod (setf data) (data (asset asset))
  (let ((resource (resource asset)))
    (if resource
        (setf (data resource) data)
        (setf (resource asset) (make-instance 'resource :asset asset :data data)))))

(defmethod matches ((a asset) (b asset))
  (and (eql (type-of a) (type-of b))
       (string-equal (name a) (name b))))

(defmethod reload ((asset asset))
  (let ((resource (resource asset)))
    (with-simple-restart (abort "Give up reloading the asset.")
      (finalize-data asset (slot-value resource 'data))
      (setf (data resource) (load-data asset)))))

(defmethod reload :around ((asset asset))
  (when (loaded-p asset)
    (if *redefining*
        (v:debug :trial.asset "Skipping reload of ~a due to redefinition context." asset)
        (call-next-method)))
  asset)

(defmethod restore ((asset asset))
  (unless (loaded-p asset)
    (setf (data asset) (load-data asset)))
  asset)

(defmethod finalize ((asset asset))
  (offload asset))

(defmethod offload ((asset asset))
  (when (loaded-p asset)
    (let ((resource (resource asset)))
      ;; Don't use the accessor as it might otherwise reload it.
      (finalize-data asset (data resource))
      (setf (data resource) NIL)
      (setf (resource asset) NIL)))
  asset)

(defmethod load-data :around ((asset asset))
  (restart-case
      (with-retry-restart (retry "Retry loading the asset's data.")
        (call-next-method))
    (use-value (value)
      :report "Enter the data to use."
      :interactive input-value
      value)))

(defmethod load-data :before ((asset asset))
  (v:info :trial.asset "~a loading data..." asset)
  (loop for (type pool name) in (dependencies asset)
        do (restore (asset type pool name))))

(defmethod finalize-data :before ((asset asset) data)
  (v:info :trial.asset "~a finalizing data..." asset))

(defmethod finalize-data :around ((asset asset) (null null))
  NIL)

(defun get-resource (type pool name)
  (let ((asset (or (asset type pool name)
                   (error "No asset of type ~s with name ~s in ~a."
                          type name pool))))
    (resource (restore asset))))

(defun update-or-create-asset (type name home pools &rest options)
  (let ((asset (asset type home name)))
    (cond (asset
           ;; Remove from all in case pools changed.
           (dolist (pool (pools))
             (leave asset pool))
           (apply #'reinitialize-instance asset :name name :home home options))
          (T
           (setf asset (apply #'make-instance type :name name :home home options))))
    (loop for pool in pools
          do (enter asset pool))
    asset))

(defmacro define-asset (type name (home &rest pools) &body options)
  `(name (update-or-create-asset ',type ',name ',home ',pools ,@options)))

(defclass asset-reference (reference)
  ((referenced-type :initform 'asset :allocation :class)))

(defmethod @=> ((type (eql 'asset)) &rest args)
  (apply #'asset args))

(defmethod serialize ((asset asset))
  `(@ asset ,(class-name (class-of asset))
            ,(name (pool asset))
            ,(name asset)))

(defclass resource ()
  ((data :initform NIL :accessor data)
   (asset :initarg :asset :accessor resource-asset))
  (:default-initargs
   :asset (error "ASSET required.")))

(defmethod initialize-instance :after ((resource resource) &key data)
  (setf (data resource) data))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type T :identity T)
    (format stream "~a::~a ~s"
            (name (home (resource-asset resource)))
            (name (resource-asset resource))
            (slot-value resource 'data))))

#-trial-optimize-resource-validity-check
(defmethod data :around ((resource resource))
  (or (call-next-method)
      (let* ((asset (resource-asset resource))
             (new (resource asset)))
        (v:severe :trial.asset "Data reference to finalized resource of ~a. Expect instability or crashes down the road."
                  asset)
        #+trial-debug-error-on-invalid-asset-reference
        (error "Illegal access to finalised resource.")
        #-trial-debug-error-on-invalid-asset-reference
        (cond ((eql resource new)
               (v:severe :trial.asset "~a has not yet been restored, forcing reload. This might work!"
                         asset resource)
               (reload asset)
               (slot-value resource 'data))
              ((not new)
               (v:severe :trial.asset "~a has not yet been restored, injecting ~a and forcing reload. This might work!"
                         asset resource)
               (setf (resource asset) resource)
               (reload asset)
               (slot-value resource 'data))
              (new
               (v:severe :trial.asset "~a has already been restored, attempting recovery by copying ~a's data. This will not end well!"
                         asset new)
               (setf (data resource) (data new)))))))

(defmethod (setf data) :after (data (resource resource))
  (tg:cancel-finalization resource)
  (when data
    (tg:finalize resource (lambda () (finalize-data (resource-asset resource) data)))))

;; Delegate
(defmethod slot-missing (class (resource resource) slot operation &optional new-value)
  (ecase operation
    (setf (setf (slot-value (resource-asset resource) slot) new-value))
    (slot-makunbound (slot-makunbound (resource-asset resource) slot))
    (slot-value (slot-value (resource-asset resource) slot))
    (slot-boundp (slot-boundp (resource-asset resource) slot))))

(defmethod @=> ((type (eql 'resource)) &rest args)
  (resource (apply #'asset args)))

(defmethod serialize ((resource resource))
  (let ((asset (resource-asset resource)))
    `(@ resource ,(class-name (class-of asset))
                 ,(name (pool asset))
                 ,(name asset))))
