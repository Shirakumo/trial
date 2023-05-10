#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pools* (make-hash-table :test 'eql))

  (defun find-pool (name &optional errorp)
    (or (gethash name *pools*)
        (when errorp (error "No pool with name ~s." name))))

  (defun (setf find-pool) (pool name)
    (setf (gethash name *pools*) pool))

  (defun remove-pool (name)
    (remhash name *pools*))

  (defun list-pools ()
    (alexandria:hash-table-values *pools*))

  (defclass pool ()
    ((name :initarg :name :accessor name)
     (base :initarg :base :accessor base)
     (assets :initform (make-hash-table :test 'eq) :accessor assets))
    (:default-initargs
     :name (error "NAME required.")))

  (defmethod print-object ((pool pool) stream)
    (print-unreadable-object (pool stream :type T)
      (format stream "~a ~s" (name pool) (base pool)))))

(defmacro define-pool (name &body initargs)
  (check-type name symbol)
  (let ((path (or *compile-file-pathname* *load-pathname*
                  (error "This needs to be compile-filed!"))))
    (setf path (merge-pathnames (getf initargs :base #p"")
                                (pathname-utils:subdirectory path "data")))
    (remf initargs :base)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cond ((find-pool ',name)
              (reinitialize-instance (find-pool ',name) ,@initargs))
             (T
              (setf (find-pool ',name) (make-instance 'pool :name ',name ,@initargs :base ,path))))
       ',name)))

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
  (merge-pathnames (base pool) (deploy:data-directory)))

(defmethod pool-path ((pool pool) pathname)
  (merge-pathnames pathname (pool-path pool NIL)))

(defmethod pool-path ((name symbol) pathname)
  (pool-path (find-pool name T) pathname))

(defmethod compile-resources ((pool pool) (source (eql T)) &rest args &key &allow-other-keys)
  (dolist (asset (list-assets pool))
    (apply #'compile-resources asset source args)))

(define-pool trial)
