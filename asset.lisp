(in-package #:org.shirakumo.fraf.trial)

(defclass placeholder-resource (resource)
  ((generator :initform (error "GENERATOR required."))
   (name :initform T)))

(defmethod print-object ((resource placeholder-resource) stream)
  (let ((asset (generator resource)))
    (print-unreadable-object (resource stream :type T)
      (if (pool asset)
          (format stream "~a/~a[~a]" (name (pool asset)) (name asset) (name resource))
          (format stream "?/~a[~a]" (name asset) (name resource))))))

(defmethod allocated-p ((resource placeholder-resource)) NIL)

(defmethod allocate ((resource placeholder-resource))
  (allocate (ensure-generated resource)))

(defmethod ensure-generated ((resource placeholder-resource))
  (load (generator resource))
  #-elide-resource-generation-checks
  (when (typep resource 'placeholder-resource)
    (error "Loading the asset~%  ~a~%did not generate the resource~%  ~a"
           (generator resource) resource))
  resource)

(defmethod ensure-generated ((resource resource))
  resource)

(defmethod unload ((resource placeholder-resource)))

(defmethod dependencies ((resource placeholder-resource))
  (list (generator resource)))

(defmethod gl-source ((resource placeholder-resource))
  (gl-source (generator resource)))

(defclass asset (resource-generator loadable)
  ((pool :initform NIL :accessor pool)
   (name :initform NIL :accessor name)
   (documentation :initarg :documentation :initform NIL)
   (input :initarg :input :initform NIL :accessor input)
   (loaded-p :initform NIL :accessor loaded-p :reader allocated-p)
   (generation-arguments :initform () :initarg :generation-arguments :accessor generation-arguments)))

(defgeneric load (asset))
(defgeneric reload (asset))
(defgeneric unload (asset))
(defgeneric list-resources (asset))
(defgeneric coerce-asset-input (asset input))

(defun // (pool asset &optional (resource T))
  (resource (asset pool asset) resource))

(define-compiler-macro // (&whole whole pool asset &optional (resource T) &environment env)
  ;; We can do this because an asset's generated resources must be updated in place.
  (if (and (constantp pool env)
           (constantp asset env))
      (if (constantp resource env)
          `(load-time-value (resource (asset ,pool ,asset) ,resource))
          `(resource (asset ,pool ,asset) ,resource))
      whole))

(defmethod shared-initialize :after ((asset asset) slots &key pool name)
  (check-type name symbol)
  (when name
    (setf (name asset) name))
  (when pool
    (setf (pool asset) (etypecase pool
                         (symbol (find-pool pool T))
                         (pool pool))))
  (when (and (pool asset) (name asset))
    (setf (asset (pool asset) name) asset)))

(defmethod reinitialize-instance :after ((asset asset) &key)
  (when (loaded-p asset)
    (reload asset)))

(defmethod update-instance-for-different-class :around ((previous asset) (current asset) &key)
  (cond ((loaded-p previous)
         (unload previous)
         (call-next-method)
         (load current))
        (T
         (call-next-method))))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~a/~a" (when (pool asset) (name (pool asset))) (name asset))))

(defmethod describe-object ((asset asset) stream)
  (call-next-method)
  (format stream "~&~%Documentation:~%~@<  ~@;~a~;~:>~&" (documentation asset T))
  (format stream "~&~%Resources:~%")
  (labels ((g< (a b)
             (cond ((and (consp a) (consp b))
                    (and (string< (car a) (car b))
                         (g< (cdr a) (cdr b))))
                   ((consp a)
                    (g< (car a) b))
                   ((consp b)
                    (g< a (car b)))
                   (T
                    (string< a b)))))
    (dolist (resource (sort (list-resources asset) #'g< :key #'name))
      (format stream "  ~s~40t~40<[~a]~>~%" (name resource) (type-of resource)))))

(defmethod documentation ((asset asset) (doc-type (eql T)))
  (slot-value asset 'documentation))

(defmethod (setf documentation) (value (asset asset) (doc-type (eql T)))
  (setf (slot-value asset 'documentation) value))

(defmethod documentation ((name cons) (doc-type (eql 'asset)))
  (documentation (asset (first name) (second name) T) T))

(defmethod (setf documentation) (value (name cons) (doc-type (eql 'asset)))
  (setf (documentation (asset (first name) (second name) T) T) value))

(defmethod resource ((asset asset) id)
  (error "The asset~%  ~a~%does not hold a resource named~%  ~a"
         asset id))

(defmethod generate-resources :around ((asset asset) (input (eql T)) &rest args)
  (apply #'generate-resources asset (input* asset) (append args (generation-arguments asset))))

(defmethod reload ((asset asset))
  (when (and (loaded-p asset) *context*)
    (with-context (*context*)
      (deallocate asset)
      (loop for resource in (enlist (apply #'generate-resources asset (input* asset) (generation-arguments asset)))
            do (dolist (dependency (dependencies resource))
                 (allocate dependency))
               (allocate resource))
      ;; KLUDGE: This kind of tight binding here really sucks.
      ;;         But I don't want to introduce a watch mechanism or something
      ;;         like that right now, so....
      (when (typep +main+ 'main)
        (let ((area (make-instance 'staging-area :loader (loader +main+))))
          (with-cleanup-on-failure (abort-commit area)
            (flet ((try (thing)
                     (when (and (typep thing 'prefab) (eq asset (prefab-asset thing)))
                       (clear thing)
                       (stage thing area))))
              (do-scene-graph (node (scene +main+)) (try node))
              (dolist (thing (to-preload (scene +main+))) (try thing))))
          (commit area T :unload NIL))))))

(defmethod load ((asset asset))
  (apply #'generate-resources asset (input* asset) (generation-arguments asset)))

(defmethod load :around ((asset asset))
  (unless (loaded-p asset)
    (v:trace :trial.asset "Loading ~a" asset)
    (with-cleanup-on-failure (deallocate asset)
      (call-next-method))))

(defmethod generate-resources :after ((asset asset) input &key)
  (setf (loaded-p asset) T))

(defmethod unload :around ((asset asset))
  (when (loaded-p asset)
    (v:trace :trial.asset "Unloading ~a" asset)
    (call-next-method)))

(defmethod unload :after ((asset asset))
  (setf (loaded-p asset) NIL))

(defmethod deallocate :after ((asset asset))
  (setf (loaded-p asset) NIL))

(defmethod allocate ((asset asset))
  (load asset))

(defmethod coerce-asset-input ((asset asset) (input (eql T)))
  (coerce-asset-input asset (input asset)))

(defmethod coerce-asset-input ((asset asset) thing)
  thing)

(defmethod coerce-asset-input ((asset asset) (path pathname))
  (let ((path (if (pool asset)
                  (pool-path (pool asset) path)
                  path)))
    (if (wild-pathname-p path)
        (directory path)
        path)))

(defmethod coerce-asset-input ((asset asset) (list list))
  (loop for item in list collect (coerce-asset-input asset item)))

(defmethod input* ((asset asset))
  (coerce-asset-input asset (input asset)))

(defun check-loaded (asset)
  (restart-case
      (unless (loaded-p asset)
        (error "The asset~%  ~a~%needs to be loaded, but is not."
               asset))
    (continue ()
      :report (lambda (s) (format s "Load ~a now" asset))
      (load asset)))
  asset)

(defmacro define-asset ((pool name) type input &rest options)
  (check-type pool symbol)
  (check-type name symbol)
  (check-type type symbol)
  (form-fiddle:with-body-options (body options documentation) options
    (assert (null body))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (ensure-instance (asset ',pool ',name NIL) ',type
                          :documentation ,documentation
                          :input ,input
                          :name ',name
                          :pool ',pool
                          :generation-arguments (list ,@options)))

       (maybe-handle-main-event 'asset-changed :changed-asset (asset ',pool ',name)))))

(trivial-indent:define-indentation define-asset (4 6 4 &body))

(defun pathname-asset-name (path &key ignore-directory)
  (flet ((rep (regex replace source)
           (cl-ppcre:regex-replace-all regex source replace)))
    (let ((name (rep "[ _\\-.]+" "-" (pathname-name path)))
          (dirs (unless ignore-directory (rest (pathname-directory path)))))
      (format NIL "~:@(~{~a/~}~a~)" dirs name))))

(defun process-asset-inputs (files &key (base *default-pathname-defaults*) (coalesce T))
  (let ((table (make-hash-table :test 'equal)))
    (flet ((reduce-filename (file)
             (or (cl-ppcre:register-groups-bind (name) ("^(.*?)(?:-\\d+)?\\..*$" (namestring file))
                   name)
                 (namestring file))))
      (loop for file in files
            for path = (pathname-utils:enough-pathname file base)
            do (push path (gethash (if coalesce (reduce-filename path) (namestring path)) table)))
      (loop for k being the hash-keys of table using (hash-value inputs)
            do (setf (gethash k table) (sort inputs #'string< :key #'namestring))))
    table))

(defun generate-assets-from-path (pool type pathname &key (package *package*) attributes ignore-directory debug exclude (coalesce T))
  (let* ((default-options (rest (find T attributes :key #'first)))
         (exclude (enlist exclude))
         (paths (process-asset-inputs
                 (loop for path in (uiop:directory* (pool-path pool pathname))
                       unless (loop for exclusion in exclude
                                    thereis (pathname-match-p path exclusion))
                       collect path)
                 :coalesce coalesce
                 :base (pool-path pool #p""))))
    (when debug
      (print (pool-path pool pathname)))
    (loop for name being the hash-keys of paths using (hash-value inputs)
          collect (let* ((name (intern (pathname-asset-name name :ignore-directory ignore-directory) package))
                         (options (append (rest (find name attributes :key #'first)) default-options)))
                    (if debug
                        (print `(define-asset (,pool ,name) ,type
                                    ,(if (rest inputs) inputs (first inputs))
                                  ,@options))
                        (ensure-instance (asset pool name NIL) type
                                         :input (if (rest inputs) inputs (first inputs))
                                         :name name
                                         :pool pool
                                         :generation-arguments options))))))

(defmacro define-assets-from-path ((pool type pathname &rest args) &body attributes)
  `(generate-assets-from-path ',pool ',type ,pathname
                              :attributes (list ,@(loop for (name . args) in attributes
                                                        collect `(list ',name ,@args)))
                              :package ,*package*
                              ,@args))

(defclass single-resource-asset (asset)
  ((resource)))

(defmethod shared-initialize :after ((asset single-resource-asset) slots &key)
  (unless (slot-boundp asset 'resource)
    (setf (slot-value asset 'resource) (make-instance 'placeholder-resource :generator asset))))

(defmethod resource ((asset single-resource-asset) (id (eql T)))
  (slot-value asset 'resource))

(defmethod list-resources ((asset single-resource-asset))
  (list (resource asset T)))

(defmethod unload ((asset single-resource-asset))
  (unload (resource asset T)))

(defmethod deallocate ((asset single-resource-asset))
  (when (allocated-p (resource asset T))
    (deallocate (resource asset T)))
  (change-class (resource asset T) 'placeholder-resource :generator asset))

(defclass multi-resource-asset (asset)
  ((resources :initform (make-hash-table :test 'equal))))

(defmethod resource ((asset multi-resource-asset) id)
  (let ((table (slot-value asset 'resources)))
    (or (gethash id table)
        (setf (gethash id table)
              (make-instance 'placeholder-resource :generator asset)))))

(defmethod list-resources ((asset multi-resource-asset))
  (loop for resource being the hash-values of (slot-value asset 'resources)
        collect resource))

(defmethod unload ((asset multi-resource-asset))
  (loop for resource being the hash-values of (slot-value asset 'resources)
        do (unload resource)))

(defmethod deallocate ((asset multi-resource-asset))
  (loop for name being the hash-keys of (slot-value asset 'resources)
        for resource being the hash-values of (slot-value asset 'resources)
        do (when (allocated-p resource)
             (deallocate resource))
           (change-class resource 'placeholder-resource :name name :generator asset)))

(defclass file-input-asset (asset)
  ())

(defmethod shared-initialize :after ((asset file-input-asset) slots &key &allow-other-keys)
  (let ((input (enlist (input asset)))
        (modified NIL))
    (loop for file in (coerce-asset-input asset input)
          for cons on input
          do (unless (probe-file file)
               (restart-case (warn 'asset-input-file-missing :file file :asset asset)
                 (use-value (value)
                   :interactive input-value
                   :report "Supply another input file"
                   (setf modified T)
                   (setf (car cons) value)))))
    (when modified (setf (input asset) input))))

(defmethod compile-resources ((asset asset) (source (eql T)) &rest args &key &allow-other-keys)
  (when (typep asset 'compiled-generator)
    (apply #'compile-resources asset (input* asset) args)))

(defmethod compile-resources ((all (eql T)) (source (eql T)) &rest args &key &allow-other-keys)
  (dolist (pool (list-pools))
    (apply #'compile-resources pool source args)))

(defmethod compile-resources ((pool symbol) (source (eql T)) &rest args &key &allow-other-keys)
  (apply #'compile-resources (find-pool pool T) source args))

(defclass full-load-asset (asset)
  ())
