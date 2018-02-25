#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *windows* ())

(defun window (name &optional (errorp T))
  (or (etypecase name
        ((and symbol (not null)) (find name *windows* :key #'name))
        (integer (nth name *windows*)))
      (when errorp (error "No window with name ~s found." name))))

(defun register-window (window)
  (when (and (name window) (window name NIL))
    (cerror "Override the window." "There already is a window with name ~s." (name window)))
  (push window *windows*)
  window)

(defun deregister-window (window)
  (setf *windows* (remove window *windows*))
  window)

(defun list-windows ()
  *windows*)

(defclass window ()
  ((name :initarg :name :accessor name))
  (:default-initargs :name NIL))

(defmethod initialize-instance :before ((window window))
  (register-window window))

(defmethod finalize ((window window))
  (deregister-window window))
