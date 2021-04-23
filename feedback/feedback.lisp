#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.feedback
  (:use #:cl)
  (:export
   #:*project*
   #:*client-args*
   #:report-files
   #:find-user-id
   #:submit-report))
(in-package #:org.shirakumo.fraf.trial.feedback)

(defvar *project*)
(defvar *client-args* ())

(defmacro error-or (&rest cases)
  (let ((id (gensym "BLOCK")))
    `(cl:block ,id
       ,@(loop for case in cases
               collect `(ignore-errors
                         (return-from ,id ,case))))))

(defmacro call (func-ish &rest args)
  (let* ((slash (position #\/ (string func-ish)))
         (package (subseq (string func-ish) 0 slash))
         (symbol (subseq (string func-ish) (1+ slash)))
         (symbolg (gensym "SYMBOL")))
    `(let ((,symbolg (find-symbol ,symbol ,package)))
       (if ,symbolg
           (funcall ,symbolg ,@args)
           (error "No such symbol ~a:~a" ,package ,symbol)))))

(defmethod report-files ()
  `(("log" ,(trial:logfile))
    ("screenshot" ,(trial:capture NIL :file (trial:tempfile)))))

(defun find-user-id ()
  (error-or
   (format NIL "~a@steam [~a]"
           (call steam/steam-id T)
           (call steam/display-name T))
   (pathname-utils:directory-name (user-homedir-pathname))
   "anonymous"))

(defun submit-report (&key (project *project*) (user (find-user-id)) (files () files-p) description version)
  (let ((files (remove-if-not #'probe-file (if files-p files (report-files)) :key #'second)))
    (handler-bind ((error (lambda (e)
                            (v:debug :trial.report e)
                            (v:error :trial.report "Failed to submit report: ~a" e))))
      (apply #'org.shirakumo.feedback.client:submit
       project user
       :version (or version (asdf:component-version (asdf:find-system project)))
       :description description
       :attachments files
       *client-args*))))

(defun trial:standalone-error-handler (err)
  (when (deploy:deployed-p)
    (v:error :trial err)
    (v:fatal :trial "Encountered unhandled error in ~a, bailing." (bt:current-thread))
    (cond ((string/= "" (or (uiop:getenv "DEPLOY_DEBUG_BOOT") ""))
           (invoke-debugger err))
          ((ignore-errors (submit-report :description (format NIL "Hard crash due to error:~%~a" err)))
           (org.shirakumo.messagebox:show (format NIL "An unhandled error occurred. A log has been sent to the developers. Sorry for the inconvenience!")
                                          :title "Unhandled Error" :type :error :modal T))
          (T
           (org.shirakumo.messagebox:show (format NIL "An unhandled error occurred. Please send the application logfile to the developers. You can find it here:~%~%~a"
                                                  (uiop:native-namestring (trial:logfile)))
                                          :title "Unhandled Error" :type :error :modal T)))
    (deploy:quit)))
