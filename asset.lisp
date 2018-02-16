#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass asset (resource)
  ((pool :initform NIL :accessor pool)
   (name :initform NIL :accessor name)
   (input :initarg :input :accessor input)))

(defmethod initialize-instance :after ((asset asset) &key pool name input)
  (check-type name symbol)
  (setf (name asset) name)
  (setf (pool asset) (etypecase pool
                       (symbol (pool pool T))
                       (pool pool)))
  (setf (asset pool name) asset))

(defmethod reinitialize-instance :after ((asset asset) &key)
  (when (loaded-p asset)
    (reload asset)))

(defmethod update-instance-for-different-class :around ((previous asset) (current asset) &key)
  ;; FIXME: Error recovery?
  (cond ((loaded-p current)
         (offload current)
         (call-next-method)
         (load current))
        (T
         (call-next-method))))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~a/~a" (name (pool asset)) (name asset))))

(defgeneric load (asset))
(defgeneric reload (asset))
(defgeneric offload (asset))
(defgeneric loaded-p (asset))

(defmethod reload ((asset asset))
  (offload asset)
  (load asset))

(defmethod load :around ((asset asset))
  (unless (loaded-p asset)
    (v:trace :trial.asset "Loading ~a/~a" (name (pool asset)) (name asset))
    (call-next-method)))

(defmethod offload :around ((asset asset))
  (when (loaded-p asset)
    (v:trace :trial.asset "Offloading ~a/~a" (name (pool asset)) (name asset))
    (call-next-method)))

(defmethod coerce-asset-input ((asset asset) (path pathname))
  (pool-path (pool asset) path))

(defmethod coerce-asset-input ((asset asset) (list list))
  (loop for item in list collect (coerce-asset-input asset item)))

(defmacro define-asset ((pool name) type input &rest options)
  (check-type pool symbol)
  (check-type name symbol)
  (check-type type symbol)
  `(let ((,name (asset ',pool ',name NIL)))
     (cond ((and ,name (eql (type-of ,name) ',type))
            (reinitialize-instance ,name :input ,input ,@options))
           (,name
            (change-class ,name ',type :input ,input ,@options))
           (T
            (setf (asset ',pool ',name)
                  (make-instance ',type :input ,input ,@options))))))

(trivial-indent:define-indentation define-asset (4 6 4 &rest))
