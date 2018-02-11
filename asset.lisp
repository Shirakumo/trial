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

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~a/~a" (name (pool asset)) (name asset))))

(defgeneric load (asset))
(defgeneric reload (asset))
(defgeneric offload (asset))
(defgeneric loaded-p (asset))

(defmethod reload ((asset asset))
  (when (loaded-p asset)
    (offload asset))
  (load asset))

(defmethod load :around ((asset asset))
  (unless (loaded-p asset)
    (v:trace :trial.asset "Loading ~a/~a" (name (pool asset)) (name asset))
    (call-next-method)))

(defmethod offload :around ((asset asset))
  (when (loaded-p asset)
    (v:trace :trial.asset "Offloading ~a/~a" (name (pool asset)) (name asset))
    (call-next-method)))
