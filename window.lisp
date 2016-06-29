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
    (entity (name name))))

(defun window (name)
  (gethash (window-name name) *windows*))

(defun (setf window) (window name)
  (etypecase window (window))
  (when (window name)
    (error "A window with the name ~s is already known!" name))
  (setf (gethash (window-name name) *windows*) window))

(defun remove-window (name)
  (remhash (window-name name) *windows*))

(define-widget window (QWidget entity)
  ())

(defmethod initialize-instance :before ((window window) &key name)
  (unless name
    (error "NAME required."))
  (setf (name window) (window-name name)))

(define-initializer (window register-window 1000)
  (setf (window (name window)) window))

(define-finalizer (window deregister-window -1000)
  (remove-window window))
