(defpackage #:org.shirakumo.fraf.trial.feedback
  (:use #:cl)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:*client-args*
   #:report-files
   #:define-report-hook
   #:find-user-id
   #:submit-report
   #:submit-snapshot))
(pushnew :trial-feedback *features*)

(in-package #:org.shirakumo.fraf.trial.feedback)

(defvar *report-hooks* ())
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

(defun report-files ()
  (let ((files ()))
    (dolist (hook *report-hooks* files)
      (with-simple-restart (continue "Ignore the hook ~s" (first hook))
        (loop for (name file) in (funcall (second hook))
              do (when (and file (probe-file file))
                   (push (list name file) files)))))))

(defmacro define-report-hook (name () &body body)
  `(setf *report-hooks* (list* (list ',name (lambda () ,@body))
                               (remove ',name *report-hooks* :key #'first))))

(define-report-hook logfile ()
  `(("log" ,(trial:logfile))))

(define-report-hook screenshot ()
  (let ((ratio (/ (trial:height trial:*context*) (trial:width trial:*context*))))
    `(("screenshot" ,(trial:capture NIL :file (trial:tempfile)
                                        :target-width 1280
                                        :target-height (floor (* ratio 1280)))))))

(defun find-user-id ()
  (error-or
   (format NIL "~a@steam [~a]"
           (call steam/steam-id T)
           (call steam/display-name T))
   (pathname-utils:directory-name (user-homedir-pathname))
   "anonymous"))

(defun submit-report (&key (project trial:+app-system+) (user (find-user-id)) (files () files-p) description (version (trial:version :app)))
  (let ((files (if files-p files
                   (handler-bind ((error #'continue))
                     (report-files)))))
    (handler-bind ((error (lambda (e)
                            (v:debug :trial.report e)
                            (v:error :trial.report "Failed to submit report: ~a" e))))
      (org.shirakumo.dns-client:query "feedback.tymoon.eu" :dns-servers org.shirakumo.dns-client:*google-servers*)
      (apply #'org.shirakumo.feedback.client:submit
             project user
             :version version
             :description description
             :attachments files
             *client-args*))))

(defun submit-snapshot (session-id session-duration snapshot-duration &key (project trial:+app-system+) (user (find-user-id)) trace (version (trial:version :app)))
  (handler-bind ((error (lambda (e)
                          (v:debug :trial.report e)
                          (v:error :trial.report "Failed to submit snapshot: ~a" e))))
    (org.shirakumo.dns-client:query "feedback.tymoon.eu" :dns-servers org.shirakumo.dns-client:*google-servers*)
    (apply #'org.shirakumo.feedback.client:submit-snapshot
           project user session-id session-duration snapshot-duration
           :version version :trace trace
           *client-args*)))

(defun error-handler (err)
  (cond ((trial:setting :debugging :dont-submit-reports)
         (trial:emessage "An unhandled error of type ~s occurred: ~a" (type-of err) err))
        ((ignore-errors (submit-report :description (format NIL "Hard crash due to error:~%~a" err)))
         (trial:emessage "An unhandled error occurred. A log has been sent to the developers. Sorry for the inconvenience!"))
        (T
         (trial:standard-error-hook err)))
  (deploy:quit))

(setf trial:*error-report-hook* #'error-handler)

(trial:define-command-line-command report (&rest report)
  (submit-report :files NIL :description (format NIL "~{~a~^ ~}" report))
  (format *query-io* "~&Report sent. Thank you!~%"))

#+linux
(trial::dont-deploy
 cl+ssl::libssl
 cl+ssl::libcrypto)
