#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *save-settings* T)
(define-global +settings-observers+ (make-hash-table :test 'equal))
(define-global +settings+ '(:language "eng"))

(defun setting-file-path ()
  (make-pathname :name "settings" :type "lisp"
                 :defaults (config-directory)))

(defun keymap-path ()
  (make-pathname :name "keymap" :type "lisp"
                 :defaults (config-directory)))

(defun map-leaf-settings (function &optional (settings +settings+))
  (labels ((recurse (node rpath)
             (loop for (k v) on node by #'cddr
                   do (if (and (consp v) (keywordp (car v)))
                          (recurse v (list* k rpath))
                          (funcall function (reverse (list* k rpath)) v)))))
    (recurse settings ())))

(defun load-keymap (&key path (name 'keymap) reset)
  (unless path
    (setf path (keymap-path))
    (ensure-directories-exist path)
    (when (or reset
              (not (probe-file path))
              (< (file-write-date path)
                 (file-write-date (merge-pathnames "keymap.lisp" (root)))))
      (uiop:copy-file (merge-pathnames "keymap.lisp" (root)) path)))
  (load-mapping path :name name))

(defun save-keymap (&key (path (keymap-path)) (name 'keymap))
  (ensure-directories-exist path)
  (save-mapping path :name name))

(defmethod load-settings (&optional (path (setting-file-path)))
  (with-error-logging (:trial.settings)
    (v:info :trial.settings "Loading settings from ~a" path)
    (with-open-file (stream path :direction :input
                                 :element-type 'character
                                 :if-does-not-exist NIL)
      (when stream
        (with-trial-io-syntax ()
          (let ((*save-settings* NIL))
            (map-leaf-settings
             (lambda (path value)
               (apply #'(setf setting) value path))
             (loop for k = (read stream NIL '#1=#:eof)
                   until (eq k '#1#)
                   collect k)))))))
  +settings+)

(defmethod save-settings (&optional (path (setting-file-path)))
  (ignore-errors
   (with-error-logging (:trial.settings)
     (v:info :trial.settings "Saving settings to ~a" path)
     (with-open-file (stream path :direction :output
                                  :element-type 'character
                                  :if-exists :supersede)
       (with-trial-io-syntax ()
         (labels ((plist (indent part)
                    (loop for (k v) on part by #'cddr
                          do (format stream "~&~v{ ~}~s " (* indent 2) '(0) k)
                             (serialise indent v)))
                  (serialise (indent part)
                    (typecase part
                      (cons
                       (cond ((keywordp (car part))
                              (format stream "(")
                              (plist (1+ indent) part)
                              (format stream ")"))
                             (T
                              (prin1 part stream))))
                      (null
                       (format stream "NIL"))
                      (T
                       (let ((*print-readably* T))
                         (prin1 part stream))))))
           (plist 0 +settings+))))))
  +settings+)

(defun setting (&rest path)
  (loop with node = (or +settings+ (load-settings))
        for key in path
        for next = (getf node key '#1=#:not-found)
        do (if (eq next '#1#)
               (return (values NIL NIL))
               (setf node next))
        finally (return (values node T))))

(defun %call-setting-observers (sub)
  (loop for (k v) on (gethash sub +settings-observers+) by #'cddr
        do (with-simple-restart (abort "Don't call the observer.")
             (handler-bind (#-trial-release (error #'invoke-debugger)
                            #+trial-release (error (lambda (e) (v:error :trial.settings e)
                                                     (invoke-restart 'abort))))
               (funcall v (apply #'setting sub))))))

(defun (setf setting) (value &rest path)
  (labels ((update (node key path)
             (setf (getf node key)
                   (if path
                       (update (getf node key) (first path) (rest path))
                       value))
             node))
    (setf +settings+ (update +settings+ (first path) (rest path)))
    (loop for i from 0 below (length path)
          for sub = (butlast path i)
          do (%call-setting-observers sub))
    (when *save-settings*
      (save-settings))
    value))

(defun observe-setting (setting name function)
  (setf (getf (gethash setting +settings-observers+) name) function))

(defun remove-setting-observer (setting name)
  (remf (gethash setting +settings-observers+) name))

(defmacro define-setting-observer (name &rest setting)
  (let ((setting (loop for part = (first setting)
                       until (listp part)
                       collect (pop setting)))
        (args (pop setting))
        (body setting))
    `(observe-setting ',setting ',name
                      (lambda ,args ,@body))))
