#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *language-change-hooks* '())
(defvar *languages* (make-hash-table :test 'equalp))
(define-global +loaded-language+ NIL)
(define-global +language-data+ NIL)

(defun languages ()
  (mapcar #'pathname-utils:directory-name
          (directory (merge-pathnames "lang/*/" (data-root)))))

(defun language ()
  (or +loaded-language+
      (load-language (or (setting :language) :eng))))

(defun try-find-language (language)
  (etypecase language
    ((member NIL :system)
     (try-find-language (system-locale:languages)))
    (symbol
     (try-find-language (string-downcase language)))
    (cons
     (loop for candidate in language
           thereis (try-find-language candidate)))
    (string
     (flet ((try (language)
              (when (language-files language)
                (return-from try-find-language (string-downcase language)))))
       (try language)
       (mapc #'try (language-codes:codes language))
       (let ((name (first (language-codes:names language))))
         (when name
           (mapc #'try (language-codes:codes name))))
       (try "eng")
       (try (first (languages)))))))

(defun language-dir (&optional (language (language)))
  (merge-pathnames (make-pathname :directory `(:relative "lang" ,(string-downcase language)))
                   (data-root)))

(defun language-files (&optional (language (language)))
  (directory (make-pathname :name :wild :type "sexp" :defaults (language-dir language))))

(defmacro define-language-change-hook (name args &body body)
  (let ((name (mksym *package* '%language-change-hook name)))
    `(progn
       ,(if args
            `(defun ,name ,args
               ,@body)
            `(defun ,name (language)
               (declare (ignorable language))
               ,@body))
       (pushnew ',name *language-change-hooks*))))

(defun load-language-file (file table)
  (with-trial-io-syntax ()
    (with-open-file (stream file)
      (loop for k = (read stream NIL)
            for v = (read stream NIL)
            while k
            do (setf (gethash k table) v))
      table)))

(defun load-language (&optional (language (setting :language)) replace)
  (let ((table (if (or replace (null +language-data+))
                   (make-hash-table :test 'eq)
                   +language-data+))
        (corrected-language (try-find-language language)))
    (if corrected-language
        (setf language corrected-language)
        (error "Could not find any suitable language for ~a" language))
    (when (or replace (null +loaded-language+) (not (equalp +loaded-language+ language)))
      (setf language (string-downcase language))
      (v:info :trial.language "Loading language ~s from ~a" language (language-dir language))
      (let ((files (language-files language)))
        (cond (files
               (dolist (file files)
                 (load-language-file file table))
               (setf (gethash language *languages*) table))
              ((null (gethash language *languages*))
               (error "No language named ~s found." language))))
      (setf +language-data+ (gethash language *languages*))
      (setf +loaded-language+ language)
      (dolist (hook *language-change-hooks* table)
        (funcall hook language)))
    language))

(defun save-language (file &optional (language (language)))
  (when +language-data+
    (setf language (string-downcase language))
    (v:info :trial.language "Saving language ~s to ~s" language (language-dir language))
    (with-trial-io-syntax ()
      (let ((order-table (make-hash-table :test 'eq))
            (i 0))
        (with-open-file (stream file
                                :direction :input
                                :if-does-not-exist NIL)
          (when stream
            (loop for k = (read stream NIL)
                  while k
                  do (read stream NIL)
                     (setf (gethash k order-table) (1- (incf i))))))
        (loop for k being the hash-keys of +language-data+
              do (unless (gethash k order-table)
                   (setf (gethash k order-table) (1- (incf i)))))
        (with-open-file (stream file
                                :direction :output
                                :if-exists :supersede)
          (loop for (k) in (sort (alexandria:hash-table-alist order-table) #'< :key #'cdr)
                for v = (gethash k +language-data+)
                do (format stream "~s ~s~%" k v)))))))

(defun language-string (identifier &optional (errorp T))
  (unless +language-data+ (load-language))
  (or (gethash identifier +language-data+)
      (unless identifier "NIL")
      (unless errorp (return-from language-string NIL))
      ;; Try loading again in case things changed.
      (progn (load-language +loaded-language+ T)
             (gethash identifier +language-data+))
      (restart-case
          (error "No language string defined for ~s" identifier)
        (retry ()
          :report "Retry now."
          (language-string identifier))
        (use-value (value)
          :report "Supply a string to use."
          value))))

(defun (setf language-string) (string identifier)
  (unless +language-data+ (load-language))
  (check-type string string)
  (check-type identifier symbol)
  (setf (gethash identifier +language-data+) string))

(defun ensure-language-string (thing)
  (etypecase thing
    (string thing)
    (symbol (language-string thing))))

(define-setting-observer load-language :language (value)
  (load-language value))

(defun @format (destination identifier &rest args)
  (format destination "~?" (language-string identifier) args))

(defun @formats (identifier &rest args)
  (format NIL "~?" (language-string identifier) args))

(defmacro @ (identifier)
    `(language-string ',identifier))

(set-dispatch-macro-character
 #\# #\@ (lambda (s c a)
           (declare (ignore c a))
           (language-string (read s T NIL T))))
