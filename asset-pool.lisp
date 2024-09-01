(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pools* (make-hash-table :test 'eql))

  (defun find-pool (name &optional errorp)
    (or (gethash name *pools*)
        (when errorp (error "No pool with name ~s." name))))

  (defun (setf find-pool) (pool name)
    (if pool
        (setf (gethash name *pools*) pool)
        (remhash name *pools*))
    pool)

  (defun remove-pool (name)
    (remhash name *pools*))

  (defun list-pools ()
    (alexandria:hash-table-values *pools*))

  (defclass pool ()
    ((name :initarg :name :accessor name)
     (base :initarg :base :accessor base)
     (documentation :initarg :documentation :initform NIL)
     (assets :initform (make-hash-table :test 'eq) :accessor assets)
     (unused-file-patterns :initform () :accessor unused-file-patterns))
    (:default-initargs
     :name (error "NAME required.")))

  (defmethod print-object ((pool pool) stream)
    (print-unreadable-object (pool stream :type T)
      (format stream "~a ~s" (name pool) (base pool)))))

(defmethod describe-object ((pool pool) stream)
  (call-next-method)
  (format stream "~&~%Documentation:~%~@<  ~@;~a~;~:>~&" (documentation pool T))
  (format stream "~&~%Assets:~%")
  (dolist (asset (sort (alexandria:hash-table-values (assets pool))
                       #'string< :key #'name))
    (format stream "  ~s~40t~40<[~a]~>~%" (name asset) (type-of asset))))

(defmethod shared-initialize :after ((pool pool) slots &key (unused-file-patterns NIL patterns-p))
  (when patterns-p (setf (unused-file-patterns pool) unused-file-patterns)))

(defmethod documentation ((pool pool) (doc-type (eql T)))
  (slot-value pool 'documentation))

(defmethod (setf documentation) (value (pool pool) (doc-type (eql T)))
  (setf (slot-value pool 'documentation) value))

(defmethod documentation ((name symbol) (doc-type (eql 'pool)))
  (documentation (find-pool name T) T))

(defmethod (setf documentation) (value (name symbol) (doc-type (eql 'pool)))
  (setf (documentation (find-pool name T) T) value))

(defun normalize-asset-file-pattern (pool pattern)
  (etypecase pattern
    (pathname
     (when (pathname-utils:absolute-p pattern)
       (error "File pattern cannot be absolute:~%  ~a" pattern))
     (when (pathname-utils:logical-p pattern)
       (error "File pattern cannot be logical:~%  ~a" pattern))
     pattern)
    (string
     (normalize-asset-file-pattern pool (pathname pattern)))
    (file-input-asset
     (normalize-asset-file-pattern pool (input pattern)))
    (symbol
     (normalize-asset-file-pattern pool (asset pool pattern T)))))

(defmethod (setf unused-file-patterns) ((patterns cons) (pool pool))
  (call-next-method (delete-duplicates
                     (loop for pat in patterns
                           collect (normalize-asset-file-pattern pool pat))
                     :test #'equal)
                    pool))

(defmacro define-pool (name &body initargs)
  (check-type name symbol)
  (let ((path (or *compile-file-pathname* *load-pathname*
                  (error "This needs to be compile-filed!"))))
    (setf path (pathname-utils:merge-pathnames*
                (getf initargs :base #p"")
                (pathname-utils:subdirectory path "data")))
    (remf initargs :base)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cond ((find-pool ',name)
              (reinitialize-instance (find-pool ',name) ,@initargs))
             (T
              (setf (find-pool ',name) (make-instance 'pool :name ',name ,@initargs :base ,path))))
       ',name)))

(defmacro define-as-unused (pool &body defs)
  `(let ((pool (find-pool ',pool T)))
     (setf (unused-file-patterns pool)
           (list* ,@(loop for def in defs collect `',def)
                  (unused-file-patterns pool)))))

(defmethod asset ((pool pool) name &optional (errorp T))
  (or (gethash name (assets pool))
      (when errorp (error "No asset with name ~s on pool ~a." name pool))))

(defmethod asset ((pool symbol) name &optional (errorp T))
  (let ((pool (find-pool pool errorp)))
    (when pool (asset pool name errorp))))

(define-compiler-macro asset (&whole whole pool name &optional (errorp T) &environment env)
  ;; We can do this because assets get updated in place rather than being recreated.
  (if (and (constantp pool env)
           (constantp name env))
      (let ((poolg (gensym "POOL")))
        `(load-time-value
          (let ((,poolg (find-pool ,pool ,errorp)))
            (when ,poolg
              (or (gethash ,name (assets ,poolg))
                  (when ,errorp (error "No asset with name ~s on pool ~a." ,name ,pool)))))))
      whole))

(defmethod (setf asset) (asset (pool symbol) name)
  (setf (asset (find-pool pool T) name) asset))

(defmethod (setf asset) ((asset asset) (pool pool) name)
  (setf (gethash name (assets pool)) asset))

(defmethod (setf asset) ((null null) (pool pool) name)
  (unload (remhash name (assets pool))))

(defmethod list-assets ((pool pool))
  (alexandria:hash-table-values (assets pool)))

(defmethod finalize ((pool pool))
  (mapc #'finalize (list-assets pool)))

(defmethod pool-path ((pool pool) (null null))
  (pathname-utils:merge-pathnames* (base pool) (deploy:data-directory)))

(defmethod pool-path ((pool pool) pathname)
  (pathname-utils:merge-pathnames* pathname (pool-path pool NIL)))

(defmethod pool-path ((name symbol) pathname)
  (pool-path (find-pool name T) pathname))

(defmethod compile-resources ((pool pool) (source (eql T)) &rest args &key &allow-other-keys)
  (dolist (asset (list-assets pool))
    (with-simple-restart (continue "Ignore ~a" asset)
      (apply #'compile-resources asset source args))))

(define-pool trial)
