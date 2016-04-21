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

(defclass resource ()
  ((data :initarg :data :reader data))
  (:default-initargs
   :data (error "DATA required.")
   :asset (error "ASSET required.")))

(defmethod initialize-instance :after ((resource resource) &key asset data)
  (tg:finalize resource (lambda () (finalize-data asset data))))

(defvar *pools* (make-hash-table :test 'eql))

(defmethod pool (name)
  (gethash (intern (string-upcase name) :KEYWORD) *pools*))

(defmethod pool ((name keyword))
  (gethash name *pools*))

(defmethod (setf pool) ((pool pool) name)
  (setf (gethash (intern (string-upcase name) :KEYWORD) *pools*) pool))

(defmethod (setf pool) ((pool pool) (name keyword))
  (setf (gethash name *pools*) pool))

(defun remove-pool (name)
  (remhash name *pools*))

(defclass asset () ()) ; forward definition
(defclass pool ()
  ((name :initarg :name :reader name)
   (base :initform NIL :accessor base)
   (assets :initform NIL :accessor assets))
  (:default-initargs
   :name (error "NAME required.")
   :base (error "BASE required.")))

(defmethod print-object ((pool pool) stream)
  (print-unreadable-object (pool stream :type T)
    (let ((*package* (find-package '#:empty-package)))
      (format stream "~s ~s" (name pool) (base pool)))))

(defmethod initialize-instance :after ((pool pool) &key base)
  (let ((prev (pool (name pool))))
    (when prev
      (setf (assets pool) (assets prev))))
  (setf (pool (name pool)) pool)
  (setf (base pool) base))

(defmethod (setf base) (thing (pool pool))
  (error "Cannot set ~s as base on ~a. Must be a pathname-designator."
         thing pool))

(defmethod (setf base) ((base string) (pool pool))
  (setf (base pool) (uiop:parse-native-namestring base)))

(defmethod (setf base) ((base pathname) (pool pool))
  (setf (slot-value pool 'base) (pathname-utils:normalize-pathname base)))

(defmethod pool ((pool pool))
  pool)

(defmethod enter ((asset asset) pool)
  (enter asset (pool pool)))

(defmethod enter ((asset asset) (pool pool))
  (leave asset pool)
  (push asset (assets pool)))

(defmethod leave ((asset asset) pool)
  (leave asset (pool pool)))

(defmethod leave ((asset asset) (pool pool))
  (setf (assets pool) (remove asset (assets pool) :test #'matches)))

(defmethod asset ((pool symbol) type name)
  (asset (or (pool pool)
             (error "No such pool ~a" pool))
         type name))

;; FIXME: Maybe optimise asset access if it turns out to be slow.
(defmethod asset (type (pool pool) name)
  (find-if (lambda (asset) (and (eql (type-of asset) type)
                                (string-equal (name asset) name)))
           (assets pool)))

(defclass asset ()
  ((name :initarg :name :accessor name)
   (home :initform NIL :accessor home)
   (resource :initform (tg:make-weak-pointer NIL) :accessor resource)
   (resource-type :initarg :resource-type :accessor resource-type))
  (:default-initargs
   :name (error "NAME required.")
   :home (error "HOME required.")
   :resource-type 'resource))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~s::~a~@[ LOADED~]"
            (name (home asset)) (name asset) (data asset))))

(defmethod initialize-instance :after ((asset asset) &key home)
  (enter asset home)
  (setf (pool asset) home))

(defmethod (setf home) (pool (asset asset))
  (setf (slot-value asset 'home) (pool pool)))

(defmethod resource ((asset asset))
  (tg:weak-pointer-value (slot-value asset 'resource)))

(defmethod (setf resource) (value (asset asset))
  (setf (slot-value asset 'resource) (tg:make-weak-pointer value)))

(defmethod data ((asset asset))
  (let ((resource (resource asset)))
    (and resource (data resource))))

(defmethod matches ((a asset) (b asset))
  (and (eql (type-of a) (type-of b))
       (string-equal (name a) (name b))))

(defmethod reload ((asset asset))
  (let ((resource (resource asset)))
    (cond (resource
           (setf (slot-value resource 'data) (load-data asset)))
          (T (restore asset)))))

(defmethod restore ((asset asset))
  (or (resource asset)
      (setf (resource asset) (make-instance (resource-type asset)
                                            :asset asset
                                            :data (load-data asset)))))

(defmethod offload :around ((asset asset))
  (let ((data (data asset)))
    (when data
      (finalize-data asset data)
      (setf (resource asset) NIL))))

(defmethod load-data :around ((asset asset))
  (restart-case
      (with-retry-restart (retry "Retry loading the asset's data.")
        (call-next-method))
    (use-value (value)
      :report "Enter the data to use."
      :interactive input-value
      value)))

(defun get-resource (type pool name)
  (let ((asset (or (asset type pool name)
                   (error "No asset of type ~s with name ~s in ~a."
                          type name pool))))
    (restore asset)))

(defmacro define-asset (type name (home &rest pools) &body options)
  (let ((asset (gensym "ASSET")))
    `(let ((,asset (make-instance
                    ',type
                    :name ',name
                    :home ',home
                    ,@options)))
       ,@(loop for pool in pools
               collect `(enter ,asset ,pool))
       ',name)))
