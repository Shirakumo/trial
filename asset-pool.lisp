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

(defstruct (pool-path (:constructor make-pool-path (pool pathname)))
  pool pathname)

(defun substitute-asset-paths (tree pool)
  (typecase tree
    (cons (cons (substitute-asset-paths (car tree) pool)
                (substitute-asset-paths (cdr tree) pool)))
    (pathname `(make-pool-path ',pool ,tree))
    (T tree)))

(defmethod coerce-input ((asset asset) (pool-path pool-path))
  (coerce-input asset (pool-path (pool-path-pool pool-path)
                                 (pool-path-pathname pool-path))))

;; FIXME: maybe there's something nicer to find the loop
(defmacro define-asset ((pool name) type inputs &rest initargs)
  (let ((scene (gensym "SCENE"))
        (asset (gensym "ASSET"))
        (inputs (substitute-asset-paths inputs pool)))
    `(let ((,scene (when (window :main) (scene (window :main))))
           (,asset (asset ',pool ',name)))
       (cond ((and ,asset (eql (type-of ,asset) ',type))
              (reinitialize-instance ,asset :inputs (list ,@inputs) ,@initargs)
              (when (and ,scene (resource ,asset))
                (issue ,scene 'load-request :asset ,asset)))
             (T
              (setf (asset ',pool ',name)
                    (make-asset ',type (list ,@inputs) ,@initargs))
              (when (and ,scene ,asset (resource ,asset))
                (issue ,scene 'load-request :asset ,asset :action 'offload)
                (issue ,scene 'load-request :asset (asset ',pool ',name) :action 'load))))
       ',name)))

(indent:define-indentation define-asset (2 4 (&whole 4 &rest 1) &body))

(values
 (define-pool trial
   :base :trial))
