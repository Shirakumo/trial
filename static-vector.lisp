#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *static-vector-map* (tg:make-weak-hash-table :weakness :key :test 'eq))

(declaim (inline mark-static-vector))
(defun mark-static-vector (vector)
  (setf (gethash vector *static-vector-map*) T)
  vector)

(defun make-static-vector (length &rest args)
  (mark-static-vector (apply #'static-vectors:make-static-vector length args)))

(define-compiler-macro make-static-vector (length &rest args)
  `(mark-static-vector (static-vectors:make-static-vector ,length ,@args)))

(declaim (inline static-vector-p))
(defun static-vector-p (vec)
  (gethash vec *static-vector-map*))

(deftype static-vector ()
  '(satisfies static-vector-p))

(declaim (inline maybe-free-static-vector))
(defun maybe-free-static-vector (vector)
  (when (static-vector-p vector)
    (static-vectors:free-static-vector vector)
    (remhash vector *static-vector-map*)))
