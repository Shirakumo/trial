#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; FIXME: configurable defaults

(in-package #:org.shirakumo.fraf.trial)

(defgeneric load (object)
  (:method-combination progn :most-specific-last))

(defmethod load progn (object)
  (v:debug :trial.asset "Loading ~a" object))

(defgeneric offload (object)
  (:method-combination progn :most-specific-first))

(defmethod offload progn (object)
  (v:debug :trial.asset "Offloaded ~a" object))

(defclass asset ()
  ((inputs :initarg :inputs :accessor inputs)
   (resource :initform NIL :initarg :resource :accessor resource))
  (:default-initargs :inputs ()))

(defmethod initialize-instance :around ((asset asset) &rest args &key input inputs)
  (when input (setf (getf args :inputs) (list* input inputs)))
  (apply #'call-next-method asset args))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T :identity T)))

(defmethod finalize-resource ((asset asset) resource)
  (finalize-resource (type-of asset) resource))

(defmethod finalize-resource :before ((type symbol) resource)
  (v:debug :trial.asset "Finalising resource ~a of type ~a"
           resource type))

(defmethod install-finalizer ((asset asset))
  (let ((type (type-of asset))
        (resource (resource asset)))
    (tg:finalize asset (lambda () (finalize-resource type resource)))))

(defmethod (setf resource) :after (value (asset asset))
  (when value
    (install-finalizer asset)))

(defmethod coerce-input ((asset asset) input)
  (error "Incompatible input type ~s for asset of type ~s."
         (type-of input) (type-of asset)))

(defmethod coerced-inputs ((asset asset))
  (loop for input in (inputs asset)
        collect (coerce-input asset input)))

(defmethod load :around ((asset asset))
  (unless (resource asset)
    (call-next-method)
    (setf (gethash asset (assets *context*)) asset))
  asset)

(defmethod offload :around ((asset asset))
  (when (resource asset)
    (tg:cancel-finalization asset)
    (call-next-method)
    (remhash asset (assets *context*)))
  asset)

(defmethod offload progn ((asset asset))
  (finalize-resource asset (resource asset))
  (setf (resource asset) NIL))

(defmethod reload ((asset asset))
  (let ((resource (resource asset)))
    (setf (resource asset) NIL)
    (with-cleanup-on-failure (setf (resource asset) resource)
      (load asset)
      (finalize-resource asset resource))
    asset))

(defmethod finalize :after ((asset asset))
  (offload asset))

(defun make-asset (type inputs &rest initargs)
  (apply #'make-instance type :inputs (enlist inputs) initargs))

(defmacro with-assets (asset-specs &body body)
  (if asset-specs
      (destructuring-bind (variable . initform) (first asset-specs)
        `(let ((,variable (make-asset ,@initform)))
           (load ,variable)
           (unwind-protect
                (with-assets ,(rest asset-specs)
                  ,@body)
             (offload ,variable))))
      `(progn ,@body)))

(defvar *asset-cache* (tg:make-weak-hash-table :weakness :key :test 'eq))

(defun clear-asset-cache ()
  (loop for table being the hash-values of *asset-cache*
        do (loop for asset being the hash-values of table
                 do (offload asset)))
  (clrhash *asset-cache*))

(defmacro with-assets* (asset-specs &body body)
  (let ((index (gensym "INDEX"))
        (table (gensym "TABLE")))
    `(let* ((,table (or (gethash ',index *asset-cache*)
                        (setf (gethash ',index *asset-cache*)
                              (make-hash-table :test 'eq))))
            ,@(loop for (variable . initform) in asset-specs
                    collect `(,variable (or (gethash ',variable ,table)
                                            (setf (gethash ',variable ,table)
                                                  (load (make-asset ,@initform)))))))
       ,@body)))
