(in-package #:org.shirakumo.fraf.trial)

(defvar *command-line-commands* ())

(defun command-line-command (name &optional (errorp T))
  (or (find name *command-line-commands* :key #'name :test #'string-equal)
      (when errorp
        (error "No command named ~s~%Known commands:~%~{  ~a~%~}"
               name (mapcar #'name *command-line-commands*)))))

(defun (setf command-line-command) (command name)
  (if command
      (setf *command-line-commands*
            (sort (list* command (remove name *command-line-commands* :key #'name :test #'string-equal))
                  #'string< :key #'name))
      (setf *command-line-commands*
            (remove name *command-line-commands* :key #'name :test #'string-equal))))

(defun format-padded (string &key (line-start 0) (line-length 80) (offset 0) (stream *query-io*))
  (loop with li = offset
        with ci = 0
        while (< ci (length string))
        do (loop while (< li line-start)
                 do (write-char #\Space stream)
                    (incf li))
           (let ((next-stop (loop for i from ci below (length string)
                                  do (when (find (char string i) '(#\Space #\Return #\Tab #\Linefeed))
                                       (return (1+ i)))
                                  finally (return i))))
             (cond ((<= (+ li (- next-stop ci)) line-length)
                    (write-string string stream :start ci :end next-stop)
                    (incf li (- next-stop ci))
                    (setf ci next-stop))
                   ((< 0 li)
                    (terpri stream)
                    (setf li 0))
                   (T
                    (write-string string stream :start ci :end (+ ci (- line-length li)))
                    (terpri stream)
                    (setf li 0)
                    (incf ci (- line-length li)))))
        finally (fresh-line stream)))

(defun command-line-print (key value)
  (format *query-io* "~@(~a~): ~a~%" key
          (typecase value
            (pathname (pathname-utils:native-namestring value))
            (T value))))

(defclass command-line-command ()
  ((name :initarg :name :accessor name)
   (help :initarg :help :initform NIL :accessor help)
   (args :initarg :args :initform NIL :accessor args)
   (func :initarg :func :accessor func)))

(defmethod describe-command ((command command-line-command) (stream stream) &key include-help (help-column 30))
  (format stream "~a" (name command))
  (destructuring-bind (&key required optional key rest) (args command)
    (cond (include-help
           (flet ((format-help (type help &optional default)
                    (when (or type help)
                      (format stream " ~vt--- ~@[(~a) ~]" help-column type)
                      (when help
                        (format-padded help :line-start (+ help-column 4)
                                            :offset (+ (+ help-column 4) (if type (+ 3 (length (string type))) 0)))))
                    (format stream "~&~@[~vt~:*~:*   Defaults to: ~a~%~]" default help-column)))
             (format *query-io* " ~vt" help-column)
             (format-padded (help command) :line-start help-column :offset help-column)
             (dolist (arg required)
               (destructuring-bind (name &optional type help) (enlist arg)
                 (format stream "~&  ~(~a~)" name)
                 (format-help type help)))
             (dolist (arg optional)
               (destructuring-bind (name &optional default type help) (enlist arg)
                 (format stream "~&  [~(~a~)]" name)
                 (format-help type help default)))
             (dolist (arg key)
               (destructuring-bind (name &optional default type help) (enlist arg)
                 (let ((names (loop for name in (cond ((not (listp name)) (list name))
                                                      ((rest name) (rest name))
                                                      (T (list name)))
                                    collect (if (= 1 (length (string name)))
                                                (format NIL "-~a" name)
                                                (format NIL "--~a" name)))))
                   (if (eql 'boolean type)
                       (format stream "~&  [~(~{~a~^|~}~)]" names)
                       (format stream "~&  [~(~{~a~^|~}~) value]" names)))
                 (format-help type help default)))
             (when rest
               (destructuring-bind (name &optional type help) (enlist rest)
                 (format stream "~&  ~(~a~)..." name)
                 (format-help type help)))))
          (T
           (dolist (arg required)
             (format stream " ~(~a~)" (unlist arg)))
           (dolist (arg optional)
             (format stream " [~(~a~)]" (unlist arg)))
           (dolist (arg key)
             (destructuring-bind (names &optional default type help) (enlist arg)
               (declare (ignore default help))
               (let* ((names (enlist names))
                      (name (if (rest names)
                                (loop with name = (second names)
                                      for other in (cddr names)
                                      do (when (< (length (string name)) (length (string other)))
                                           (setf name other))
                                      finally (return (string name)))
                                (string (first names)))))
                 (if (= 1 (length name))
                     (setf name (format NIL "-~a" name))
                     (setf name (format NIL "--~a" name)))
                 (if (eql type 'boolean)
                     (format stream " [~(~a~)]" name)
                     (format stream " [~(~a~) value]" name)))))
           (when rest
             (format stream " ~(~a~)..." (unlist rest)))))))

(defmethod describe-command ((command command-line-command) (stream null) &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'describe-command command stream args)))

(defmethod describe-command (command target &rest args &key &allow-other-keys)
  (apply #'describe-command (command-line-command command) target args))

(defmethod parse-command-line-type (value (type null))
  value)

(defmethod parse-command-line-type (value (type (eql 'string)))
  value)

(defmethod parse-command-line-type (value (type (eql 'boolean)))
  (find value '("true" "t" "y" "yes" "on") :test #'string-equal))

(defmethod parse-command-line-type (value (type (eql 'pathname)))
  (pathname-utils:parse-native-namestring value))

(defmethod parse-command-line-type (value (type (eql 'integer)))
  (parse-integer value))

(defmethod parse-command-line-type (value (type (eql 'keyword)))
  (intern (string-upcase value) "KEYWORD"))

(defmethod parse-command-line-type (value (type (eql 'symbol)))
  (intern (string-upcase value)))

(defmethod invoke-command ((command command-line-command) &rest args)
  (destructuring-bind (&key required optional key rest) (args command)
    (let ((origargs args)
          (reqargs ())
          (optargs ())
          (keyargs ())
          (resargs ()))
      (labels ((fail (reason &rest format-args)
                 (error "~? in argument list~%  ~a~%to satisfy command~%  ~a"
                        reason format-args origargs (describe-command command NIL)))
               (add-key (name value)
                 (cond (value
                        (push value keyargs))
                       (args
                        (push (pop args) keyargs))
                       (T
                        (fail "Keyword argument ~a requires a value, but none was provided" name)))
                 (push (intern (string name) "KEYWORD") keyargs))
               (handle-key (name &optional value)
                 (loop for (var default type) in key
                       do (when (if (and (listp var) (rest var))
                                    (find name (rest var) :test #'string-equal)
                                    (string-equal var name))
                            (if (eql type 'boolean)
                                (add-key var (if value (parse-command-line-type value type) (not default)))
                                (add-key var (parse-command-line-type value type)))
                            (return))
                       finally (fail "Unknown keyword argument ~a" name))))
        (when args
          (loop for arg = (pop args)
                do (cond ((and (< 2 (length arg)) (string= "--" arg :end2 2))
                          (let ((eql (position #\= arg)))
                            (handle-key (subseq arg 2 (or eql (length arg)))
                                        (when eql (subseq arg (1+ eql))))))
                         ((and (< 1 (length arg)) (string= "-" arg :end2 1))
                          (loop for i from 1 below (length arg)
                                do (handle-key (char arg i))))
                         ((< (length reqargs) (length required))
                          (destructuring-bind (name &optional type help) (enlist (elt required (length reqargs)))
                            (declare (ignore name help))
                            (push (parse-command-line-type arg type) reqargs)))
                         ((< (length optargs) (length optional))
                          (destructuring-bind (name &optional default type help) (enlist (elt optional (length optargs)))
                            (declare (ignore name default help))
                            (push (parse-command-line-type arg type) optargs)))
                         (rest
                          (destructuring-bind (name &optional type help) (enlist rest)
                            (declare (ignore name help))
                            (push (parse-command-line-type arg type) resargs)))
                         (T
                          (fail "Superfluous arguments")))
                while args)))
      (apply (func command) (append (nreverse reqargs) (nreverse optargs) keyargs (when resargs (list '&rest (nreverse rest))))))))

(defmethod invoke-command (command &rest args)
  (apply #'invoke-command (command-line-command command) args))

(defmacro define-command-line-command (name args &body body)
  (form-fiddle:with-body-options (body other help) body
    (declare (ignore other))
    (let ((keys (mapcar #'enlist (lambda-fiddle:collect-for-keyword '&key args))))
      `(setf (command-line-command ',name)
             (make-instance 'command-line-command
                            :name ,(string-downcase name)
                            :help ,help
                            :args ',(list :required (lambda-fiddle:required-lambda-vars args)
                                          :optional (lambda-fiddle:collect-for-keyword '&optional args)
                                          :key keys
                                          :rest (lambda-fiddle:rest-lambda-var args))
                            :func (lambda (,@(loop for arg in args until (or (eql arg '&rest) (eql arg '&key)) collect (unlist arg))
                                           &key
                                             ;; We need to specially handle keys to remove the aliases and type
                                             ,@(loop for (name default) in keys
                                                     collect `(,(unlist name) ,default))
                                           ;; We convert &rest to a key argument to allow the mixing with other key args
                                           ;; as is more typical of command line arguments.
                                             ,@(when (find '&rest args)
                                                 `(((&rest ,(unlist (lambda-fiddle:rest-lambda-var args))) NIL))))
                                    ,@body))))))

(defun command-line-toplevel (&optional (args (uiop:command-line-arguments)))
  (when args
    (load-settings)
    (handler-case (apply #'invoke-command args)
      (error (e)
        (format *error-output* "~&Error: ~a~%" e)
        (uiop:quit :code 1)))
    (uiop:quit)))

(define-command-line-command help (&optional (command NIL NIL "The command to describe"))
  :help "Display help about the program or a subcommand."
  (cond (command
         (let ((command (command-line-command command)))
           (describe-command command *query-io* :include-help T)
           (format *query-io* "~&~%~a~&" (help command))))
        (T
         (format *query-io* "~a ~a~%~%== Commands:~%" (self) (version :app))
         (dolist (command *command-line-commands*)
           (describe-command command *query-io* :include-help T)
           (format *query-io* "~&~%"))
         (invoke-command "copyright"))))

(define-command-line-command copyright ()
  :help "Display copyright information"
  (format *query-io* "© ~d ~a, all rights reserved"
          #.(nth-value 5 (get-decoded-time)) +app-vendor+))

(define-command-line-command configure-controller ()
  :help "Change a gamepad's button mapping configuration"
  (gamepad::configurator-main))

(define-command-line-command system ()
  :help "Print information about the current system and application"
  (command-line-print :app +app-system+)
  (command-line-print :vendor +app-vendor+)
  (command-line-print :version #.(version :app))
  (command-line-print :hash #.(version :binary))
  (command-line-print :trial #.(version :trial))
  (command-line-print :language (or (setting :language) (first (system-locale:languages)) :eng))
  (command-line-print :username (system-username))
  (command-line-print :machine (format NIL "~a ~a ~a" (machine-type) (machine-version) (machine-instance)))
  (command-line-print :system (format NIL "~a ~a" (software-type) (software-version)))
  (command-line-print :lisp (format NIL "~a ~a" (lisp-implementation-type) (lisp-implementation-version)))
  (command-line-print :time (format-timestring))
  (when *context* (context-info *context* :stream *query-io*)))

(define-command-line-command paths (&optional (path NIL keyword "The path to open"))
  :help "Show relevant paths to the application"
  (flet ((open* (file)
           (format *query-io* "~&~a~%" (pathname-utils:native-namestring file))
           (open-in-file-manager file)))
    (ecase path
      ((NIL)
       (command-line-print :self (self))
       (command-line-print :log-file (logfile))
       (command-line-print :data-root (data-root))
       (command-line-print :config-directory (config-directory))
       (command-line-print :temp-directory (tempdir))
       (command-line-print :home-directory (user-homedir-pathname)))
      ((:self :executable) (open* (self)))
      ((:log :logfile :log-file) (open* (logfile)))
      ((:data :data-root) (open* (data-root)))
      ((:config :config-directory) (open* (config-directory)))
      ((:temp :temp-directory) (open* (tempdir)))
      ((:home :home-directory) (open* (user-homedir-pathname))))))

(define-command-line-command clear-config (&key ((quiet :quiet :q) NIL boolean "Whether to delete all files quietly"))
  :help "Clear all configuration and setting files."
  (flet ((prompt ()
           (format *query-io* "This will delete all configuration, setting, and save files for ~a.~%Are you sure? [y/N]~%" +app-system+)
           (string-equal "y" (read-line *query-io*))))
    (if (or quiet (prompt))
        (org.shirakumo.filesystem-utils:delete-directory (config-directory))
        (format *query-io* "Aborting."))))

(define-command-line-command version ()
  :help "Show the application version"
  (format *query-io* "~a ~a" (version :app) (version :binary)))

(define-command-line-command eval (&key ((load :load :l) NIL pathname "A file to load")
                                        ((print :print :p) NIL boolean "Whether to print the results of the forms")
                                        ((verbose :verbose :v) NIL boolean "Whether to print the forms being executed")
                                        &rest (forms NIL "Lisp forms to evaluate"))
  :help "Evaluate lisp forms"
  (when load
    (when verbose (format *query-io* "~&; Loading ~a~%" load))
    (cl:load load :verbose verbose))
  (dolist (form forms)
    (with-input-from-string (stream form)
      (loop for form = (read stream NIL #1='#:EOF)
            until (eq form #1#)
            do (when verbose (format *query-io* "~&; Evaluating ~a~%" load))
               (let ((values (multiple-value-list (eval form))))
                 (when print (format *query-io* "~{~&~a~%~}" values)))))))

;;; Let's build a repl real quick lol

(defun package-abbreviation (package)
  (let ((shortest (package-name package)))
    (dolist (name (package-nicknames package))
      (when (< (length name) (length shortest))
        (setf shortest name)))
    (let ((dot (position #\. shortest :from-end T)))
      (if dot
          (subseq shortest (1+ dot))
          shortest))))

(defun handle-repl-error (error &optional (stream *query-io*))
  (let ((env (dissect:capture-environment error)))
    (format stream "~a~&   [Condition of type ~s]~&~%" error (type-of error))
    (dissect:present-object (dissect:environment-restarts env) stream)
    (loop (format *query-io* "~&~a (DEBUGGER)> " (package-abbreviation *package*))
          (finish-output stream)
          (catch 'read-exit
            (let ((form (repl-read stream)))
              (typecase form
                ((integer 0)
                 (dissect:invoke (nth form (dissect:environment-restarts env))))
                (symbol
                 (loop for restart in (dissect:environment-restarts env)
                       do (when (string-equal form (dissect:name restart))
                            (dissect:invoke restart)))
                 (cond ((find form '("b" "bt" "backtrace") :test #'string-equal)
                        (dissect:present (dissect:environment-stack env) stream))
                       ((find form '("a" "abort") :test #'string-equal)
                        (abort error))
                       ((find form '("c" "continue") :test #'string-equal)
                        (continue error))
                       ((find form '("q" "quit" "e" "exit") :test #'string-equal)
                        (throw 'repl-exit NIL))
                       (T
                        (repl-print (repl-eval form stream) stream))))
                (T
                 (repl-print (repl-eval form stream) stream))))))))

(defun repl-read (&optional (stream *query-io*))
  (restart-case
      (handler-bind ((error (lambda (e) (handle-repl-error e stream))))
        (let ((form (read stream NIL #1='#:EOF)))
          (if (eq form #1#)
              (throw 'repl-exit NIL)
              form)))
    (abort ()
      :report "Abort reading"
      (throw 'read-exit NIL))))

(defun repl-eval (form &optional (stream *query-io*))
  (with-simple-restart (abort "Abort evaluation")
    (handler-bind ((error (lambda (e) (handle-repl-error e stream))))
      (let* ((- form)
             (values (multiple-value-list (eval form))))
        (shiftf *** ** * (first values))
        (shiftf /// cl:// / values)
        (shiftf +++ ++ + form)
        values))))

(defun repl-print (values &optional (stream *query-io*))
  (format stream "~{~&~a~%~}" values))

(define-command-line-command repl (&key ((package :package :p) NIL NIL "The package to start with"))
  :help "Run a read-eval-print-loop"
  (catch 'repl-exit
    (loop with *package* = (if package
                               (or (find-package package)
                                   (find-package (string-upcase package))
                                   (error "No such package ~s" package))
                               *package*)
          with *** = NIL and ** = NIL and * = nil
          with /// = NIL and // = NIL and / = nil
          with +++ = NIL and ++ = NIL and + = nil
          do (format *query-io* "~&~a> " (package-abbreviation *package*))
             (finish-output *query-io*)
             (catch 'read-exit
               (repl-print (repl-eval (repl-read)))))))
