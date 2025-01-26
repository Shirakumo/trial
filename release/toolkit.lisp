(in-package #:org.shirakumo.fraf.trial.release)

(defparameter *config*
  (copy-tree
   '(:build (:features ()
             :build-arguments ()
             :dynamic-space-size 4096
             :linux "sbcl-lin"
             :windows "sbcl-win"
             :macos "sbcl-mac"
             :targets (:linux :windows)
             :prune ()
             :copy ())
     :upload (:targets (:steam))
     :depots (:all ("**/*.*"))
     :bundles (:default (:depots T :file-format "~a-~*~a"))
     :itch (:user "CONFIGURE-ME"
            :project NIL)
     :steam (:branch "default"
             :preview NIL
             :user "CONFIGURE-ME"
             :password NIL)
     :system "CONFIGURE-ME"
     :output "CONFIGURE-ME")))

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

(defun set-config (config)
  (map-leaf-config
   (lambda (path value)
     (apply #'(setf config) value path))
   config))

(defmacro configure (&body config)
  `(set-config (append ',config '(:output ,(or *compile-file-pathname* *load-pathname*)))))

(defun coerce-program-arg (arg)
  (etypecase arg
    (string arg)
    (pathname (pathname-utils:native-namestring arg))
    (real (princ-to-string arg))))

(defun run (program &rest args)
  (assert (= 0 (sb-ext:process-exit-code (sb-ext:run-program program (mapcar #'coerce-program-arg args) :search T :output *standard-output*)))))

(defun run* (program &rest args)
  (with-output-to-string (out)
    (assert (= 0 (sb-ext:process-exit-code (sb-ext:run-program program (mapcar #'coerce-program-arg args) :search T :output out))))))

(defun envvar-password (name &optional field)
  (uiop:getenv (format NIL "TRIAL_RELEASE_~:@(~a~@[_~a~]~)" name field)))

(defun get-password (name &optional field)
  (let ((candidates (cl-ppcre:split "\\n+" (run* "pass" "find" name))))
    (when (cdr candidates)
      (let* ((entry (subseq (second candidates) (length "├── ")))
             (data (run* "pass" "show" entry))
             (lines (cl-ppcre:split "\\n+" data)))
        (if (null field)
            (first lines)
            (loop for line in (rest lines)
                  for (key val) = (cl-ppcre:split " *: *" line :limit 2)
                  do (when (string-equal key field)
                       (return val))))))))

(defun query-password (name &optional field)
  (format *query-io* "~&Enter ~a password~@[ [~a]~]:~%> " name field)
  (read-line *query-io*))

(defun password (name &optional field)
  (or (envvar-password name field)
      (get-password name field)
      (query-password name field)))

(defun list-paths (base &rest paths)
  (loop for path in paths
        for wild = (merge-pathnames path (make-pathname :directory '(:relative :wild-inferiors)))
        append (directory (merge-pathnames wild base))))

(defun file-replace (in out replacements)
  (let ((content (alexandria:read-file-into-string in)))
    (loop for (search replace) in replacements
          do (setf content (cl-ppcre:regex-replace-all search content replace)))
    (alexandria:write-string-into-file content out :if-exists :supersede)))

(defun prune (file)
  (cond ((listp file)
         (mapc #'prune file))
        ((wild-pathname-p file)
         (prune (directory file)))
        (T
         (org.shirakumo.filesystem-utils:ensure-deleted file))))

(defun copy (file target)
  (cond ((wild-pathname-p file)
         (loop for file in (directory file)
               do (copy file (merge-pathnames file target))))
        ((pathname-utils:directory-p file)
         (org.shirakumo.filesystem-utils:copy-file file target :replace T :skip-root T))
        (T
         (ensure-directories-exist target)
         (org.shirakumo.filesystem-utils:copy-file file target :replace T))))
