#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *windows* (make-hash-table :test 'equalp))

(defun window-name (name)
  (etypecase name
    (symbol name)
    (window (name name))))

(defun window (name)
  (gethash (window-name name) *windows*))

(defun list-windows ()
  (loop for window being the hash-values of *windows* collect window))

(defun (setf window) (window name)
  (etypecase window (window))
  (when (window name)
    (error "A window with the name ~s is already known!" name))
  (setf (gethash (window-name name) *windows*) window))

(defun remove-window (name)
  (remhash (window-name name) *windows*))

(define-widget window (QWidget)
  ((name :reader name)))

(defmethod initialize-instance :before ((window window) &key name)
  (setf (slot-value window 'name) (or name (class-name (class-of window)))))

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type T)
    (format stream "~s" (name window))))

(define-initializer (window register-window 1000)
  (setf (window (name window)) window))

(define-finalizer (window deregister-window -1000)
  (remove-window window))
