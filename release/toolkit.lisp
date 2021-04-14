(in-package #:org.shirakumo.fraf.trial.release)

(defvar *config* '(:build (:features ()
                           :build-arguments ()
                           :dynamic-space-size 4096
                           :linux "sbcl-lin"
                           :windows "sbcl-win"
                           :macos "sbcl-mac"
                           :targets '(:linux :windows))
                   :itch (:user "CONFIGURE-ME"
                          :project NIL)
                   :steam (:branch "developer"
                           :preview NIL
                           :user "CONFIGURE-ME"
                           :password NIL)
                   :system "CONFIGURE-ME"
                   :output "CONFIGURE-ME"))

(defun map-leaf-config (function &optional (config *config*))
  (labels ((recurse (node rpath)
             (loop for (k v) on node by #'cddr
                   do (if (and (consp v) (keywordp (car v)))
                          (recurse v (list* k rpath))
                          (funcall function (reverse (list* k rpath)) v)))))
    (recurse config ())))

(defun config (&rest path)
  (loop with node = *config*
        for key in path
        for next = (getf node key '#1=#:not-found)
        do (if (eq next '#1#)
               (return (values NIL NIL))
               (setf node next))
        finally (return (values node T))))

(defun (setf config) (value &rest path)
  (labels ((update (node key path)
             (setf (getf node key)
                   (if path
                       (update (getf node key) (first path) (rest path))
                       value))
             node))
    (setf *config* (update *config* (first path) (rest path)))
    value))

(defun set-config (&rest config)
  (map-leaf-config
   (lambda (path value)
     (apply #'(setf config) value path))
   config))

(defmacro configure (&body config)
  `(set-config (append ',config '(:output ,(or *compile-file-pathname* *load-pathname*)))))

(defun run (program &rest args)
  (assert (= 0 (sb-ext:process-exit-code (sb-ext:run-program program args :search T :output *standard-output*)))))

(defun run* (program &rest args)
  (with-output-to-string (out)
    (assert (= 0 (sb-ext:process-exit-code (sb-ext:run-program program args :search T :output out))))))

(defun get-password (name)
  (ignore-errors
   (let ((candidates (cl-ppcre:split "\\n+" (run* "pass" "find" name))))
     (when (cdr candidates)
       (let* ((entry (subseq (second candidates) (length "├── ")))
              (data (run* "pass" "show" entry)))
         (first (cl-ppcre:split "\\n+" data)))))))

(defun query-password (name)
  (format *query-io* "~&Enter ~a password:~%> " name)
  (read-line *query-io*))

(defun password (name)
  (or (get-password name)
      (query-password name)))

(defun list-paths (base &rest paths)
  (loop for path in paths
        for wild = (merge-pathnames path (make-pathname :directory '(:relative :wild-inferiors)))
        append (directory (merge-pathnames wild base))))

(defun file-replace (in out replacements)
  (let ((content (alexandria:read-file-into-string in)))
    (loop for (search replace) in replacements
          do (setf content (cl-ppcre:regex-replace-all search content replace)))
    (alexandria:write-string-into-file content out :if-exists :supersede)))
