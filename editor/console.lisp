#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor-console
  (:nicknames #:org.shirakumo.fraf.trial.editor.console)
  (:use #:cl+qt)
  (:export #:console))
(in-package #:org.shirakumo.fraf.trial.editor.console)
(in-readtable :qtools)

(define-widget console (QTextEdit qui:repl)
  ((main :initarg :main :accessor main)
   (pkg :initarg :pkg :accessor pkg))
  (:default-initargs
    :pkg (find-package '#:trial-user)
    :main (error "MAIN required.")))

;; Incapacitate the thread
(defmethod qui:repl-eval-loop ((console console)))

(defmethod qui:repl-eval ((console console) string)
  (let ((*package* (pkg console)))
    (unwind-protect
         (restart-case
             (handler-bind ((error (lambda (err)
                                     (qui:invoke-gui-debugger err)
                                     (invoke-restart 'abort err))))
               (destructuring-bind (type data package)
                   (trial::funcall-in-scene
                    (trial::scene (main console))
                    (wrap-eval-func string)
                    :bindings (make-bindings console))
                 (setf *package* package)
                 (case type
                   (:success (list type data package))
                   (:failure
                    ;; Ensure we only have local restarts
                    (setf (dissect:environment-restarts data) (dissect:restarts))
                    (qui:invoke-gui-debugger data)
                    (invoke-restart 'abort (dissect:environment-condition data))))))
           (abort (&optional err)
             :report "Abort the evaluation request."
             (list :failure (make-condition 'simple-error :format-control "Evaluation aborted~@[ on ~a~]"
                                                          :format-arguments (list (type-of err)))
                   *package*)))
      (setf (pkg console) *package*))))

(defun wrap-eval-func (string)
  (let* ((form (read-from-string string))
         (func (compile NIL `(lambda () (progn ,form)))))
    (lambda ()
      (block NIL
        (handler-bind ((error (lambda (err)
                                (return
                                  (list :failure (dissect:capture-environment err) *package*)))))
          (unwind-protect
               (let ((values (multiple-value-list (funcall func))))
                 (shiftf /// // / values)
                 (shiftf *** ** * (first values))
                 (list :success values *package*))
            (shiftf +++ ++ + form)))))))

(defun make-bindings (console)
  `((*terminal-io* ,(qui::output-stream console))
    (*standard-output* ,(qui::output-stream console))
    (*error-output* ,(qui::error-stream console))
    (*query-io* ,(qui::output-stream console))
    (*trace-output* ,(qui::output-stream console))
    (*debug-io* ,(qui::output-stream console))
    (*package* ,*package*)))
