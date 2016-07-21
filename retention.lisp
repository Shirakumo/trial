#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *retention-functions* (make-hash-table :test 'eql))
(defvar *retention-table* (make-hash-table :test 'eq))

(defun retained (type id)
  (let ((table (gethash type *retention-table*)))
    (when table (gethash id table))))

(defun (setf retained) (bool type id)
  (let ((table (gethash type *retention-table*)))
    (when table (setf (gethash id table) bool)))
  bool)

(defun clear-retained ()
  (loop for table being the hash-values of *retention-table*
        do (clrhash table)))

(defun retention-function (type)
  (gethash type *retention-functions*))

(defun (setf retention-function) (func type)
  (setf (gethash type *retention-functions*) (etypecase func (function func))))

(defun remove-retention-function (type)
  (remhash type *retention-functions*))

(defun retain-event (event)
  (unless (typep event 'tick)
    (loop for v being the hash-values of *retention-functions*
          do (funcall v event))))

(defmacro define-retention (type (ev &rest args) &body body)
  `(progn (unless (gethash ',type *retention-table*)
            (setf (gethash ',type *retention-table*)
                  (make-hash-table :test 'eq)))
          (setf (retention-function ',type)
                (lambda (,ev)
                  (declare (type event ,ev))
                  (with-slots ,args ,ev
                    ,@body)))))

(defmacro define-coupled-retention (type &body body)
  (let ((ev (gensym "EVENT")))
    (destructuring-bind (start start-args &body start-body) (first body)
      (destructuring-bind (end end-args &body end-body) (second body)
        `(define-retention ,type (,ev)
           (flet ((,start (,start)
                    (declare (type event ,start))
                    (with-slots ,start-args ,start
                      ,@start-body))
                  (,end (,end)
                    (declare (type event ,end))
                    (with-slots ,end-args ,end
                      ,@end-body)))
             (declare (inline ,start ,end))
             (cond ((typep ,ev ',start)
                    (setf (retained ',type (,start ,ev)) T))
                   ((typep ,ev ',end)
                    (setf (retained ',type (,end ,ev)) NIL)))))))))

(defmacro define-uniform-retention (type (start end &rest args) &body body)
  `(define-coupled-retention ,type (,start ,args ,@body) (,end ,args ,@body)))

(define-uniform-retention key (key-press key-release key)
  key)

(define-uniform-retention mouse (mouse-press mouse-release button)
  button)

(define-uniform-retention gamepad (gamepad-press gamepad-release button)
  button)
