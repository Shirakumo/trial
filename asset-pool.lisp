#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod coerce-base (base)
  (destructuring-bind (base &rest sub) (if (listp base) base (list base))
    (if *standalone*
        (merge-pathnames (format NIL "pool/~(~a~)/~{~a/~}" base sub) (deploy:data-directory))
        (merge-pathnames (format NIL "data/~{~a/~}" sub) (asdf:system-source-directory base)))))

(defvar *pools* (make-hash-table :test 'eql))

(defun pool (name &optional errorp)
  (or (gethash name *pools*)
      (when errorp (error "No pool with name ~s." name))))

(defun (setf pool) (pool name)
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
   :name (error "NAME required.")
   :base (error "BASE required.")))

(defmethod print-object ((pool pool) stream)
  (print-unreadable-object (pool stream :type T)
    (format stream "~a ~s" (name pool) (base pool))))

(defmacro define-pool (name &body initargs)
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cond ((pool ',name)
            (reinitialize-instance (pool ',name) ,@initargs))
           (T
            (setf (pool ',name) (make-instance 'pool :name ',name ,@initargs))))
     ',name))

(defmethod asset ((pool pool) name &optional (errorp T))
  (or (gethash name (assets pool))
      (when errorp (error "No asset with name ~s on pool ~a." name pool))))

(defmethod asset ((pool symbol) name &optional (errorp T))
  (let ((pool (pool pool errorp)))
    (when pool (asset pool name errorp))))

(defmethod (setf asset) ((asset asset) (pool pool) name)
  (setf (gethash name (assets pool)) asset))

(defmethod (setf asset) ((asset asset) (pool symbol) name)
  (setf (asset (pool pool T) name) asset))

(defmethod list-assets ((pool pool))
  (alexandria:hash-table-values (assets pool)))

(defmethod finalize ((pool pool))
  (mapc #'finalize (list-assets pool)))

(defmethod pool-path ((pool pool) (null null))
  (coerce-base (base pool)))

(defmethod pool-path ((pool pool) pathname)
  (merge-pathnames pathname (coerce-base (base pool))))

(defmethod pool-path ((name symbol) pathname)
  (pool-path (pool name T) pathname))

(defclass load-request (event)
  ((asset :initarg :asset)
   (action :initarg :action :initform 'reload)))

(eval-when (:load-toplevel :execute)
  (define-pool trial
    :base :trial))
