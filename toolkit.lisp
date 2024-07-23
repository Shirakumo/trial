(in-package #:org.shirakumo.fraf.trial)

(defvar *native-array-element-types*
  (remove T (remove-duplicates
             (mapcar #'upgraded-array-element-type
                     (append '(fixnum short-float single-float double-float long-float)
                             (loop for i from 1 to 64 collect `(signed-byte ,i))
                             (loop for i from 1 to 64 collect `(unsigned-byte ,i))
                             '(base-char extended-char character)))
             :test #'equal)))

(defmacro define-global (name value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+sbcl (sb-ext:defglobal ,name ,value)
     #-sbcl (defvar ,name ,value)
     (setf ,name ,value)))

(define-global +app-vendor+ "shirakumo")
(define-global +app-system+ "trial")
(define-global +main+ NIL)

(defconstant F-PI (float PI 0f0))
(defconstant F-2PI (float (* 2 PI) 0f0))
(defconstant F-PI/2 (float (* 0.5 PI) 0f0))

(defun git-repo-commit (dir)
  (flet ((file (path)
           (merge-pathnames path dir))
         (trim (string)
           (string-trim '(#\Return #\Linefeed #\Space) string)))
    (when (probe-file (file ".git/HEAD"))
      (let* ((head (trim (alexandria:read-file-into-string (file ".git/HEAD"))))
             (path (subseq head (1+ (or (position #\Space head) -1)))))
        (cond ((probe-file (file (merge-pathnames path ".git/")))
               (trim (alexandria:read-file-into-string (file (merge-pathnames path ".git/")))))
              ((probe-file (file ".git/packed-refs"))
               (with-open-file (stream (file ".git/packed-refs"))
                 (loop for line = (read-line stream NIL NIL)
                       while line
                       do (when (search path line :start2 40)
                            (return (subseq line 0 (position #\Space line))))))))))))

(defun self ()
  (first (uiop:raw-command-line-arguments)))

(defmethod version ((_ (eql :app)))
  (let ((commit #+asdf (git-repo-commit (asdf:system-source-directory +app-system+))))
    (format NIL "~a~@[-~a~]"
            #+asdf (asdf:component-version (asdf:find-system +app-system+))
            #-asdf "?"
            (when commit (subseq commit 0 7)))))

(defmethod version ((_ (eql :trial)))
  (let ((commit #+asdf (git-repo-commit (asdf:system-source-directory :trial))))
    (format NIL "~a~@[-~a~]"
            #+asdf (asdf:component-version (asdf:find-system :trial))
            #-asdf "?"
            (when commit (subseq commit 0 7)))))

(defmethod version ((_ (eql :binary)))
  (let ((self (self)))
    (if (and self (uiop:file-exists-p self))
        (with-output-to-string (out)
          (loop for o across (sha3:sha3-digest-file self :output-bit-length 224)
                do (format out "~2,'0X" o)))
        "?")))

(let ((cache NIL))
  (defun data-root (&optional (app +app-system+))
    (if (deploy:deployed-p)
        (deploy:runtime-directory)
        (or cache #+asdf (setf cache (asdf:system-source-directory app))))))

(defgeneric coerce-object (object type &key))

(defmethod coerce-object (object type &key)
  (coerce object type))

(defmethod coerce-object :around (object type &key)
  (if (eql (type-of object) type)
      object
      (call-next-method)))

(defgeneric finalize (object))

(defmethod finalize :before ((object standard-object))
  (v:debug :trial "Finalizing ~a" object))

(defmethod finalize (object)
  object)

(defun round-to (base number)
  (* base (ceiling number base)))

(defun gl-property (name)
  (handler-case (gl:get* name)
    (error (err) (declare (ignore err))
      :unavailable)))

(defmethod apply-class-changes ((class standard-class)))

(defmethod apply-class-changes :before ((class standard-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super))))

(defmethod apply-class-changes :after ((class standard-class))
  (make-instances-obsolete class)
  (dolist (sub (c2mop:class-direct-subclasses class))
    (apply-class-changes sub)))

(declaim (inline current-time))
(defun current-time ()
  (declare (optimize speed (safety 0)))
  (multiple-value-bind (s ms) (org.shirakumo.precise-time:get-monotonic-time)
    (let* ((s (logand s (1- (expt 2 62))))
           (ms (logand ms (1- (expt 2 62)))))
      (declare (type (unsigned-byte 62) s ms))
      (+ s (* ms (load-time-value (coerce (/ org.shirakumo.precise-time:MONOTONIC-TIME-UNITS-PER-SECOND) 'double-float)))))))

(defmacro undefmethod (name &rest args)
  (flet ((lambda-keyword-p (symbol)
           (find symbol lambda-list-keywords)))
    (destructuring-bind (qualifiers args) (loop for thing = (pop args)
                                                until (listp thing)
                                                collect thing into qualifiers
                                                finally (return (list qualifiers thing)))
      `(let ((method (find-method
                      #',name
                      ',qualifiers
                      (mapcar #'find-class
                              ',(loop for arg in args
                                      until (lambda-keyword-p arg)
                                      collect (if (listp arg) (second arg) T)))
                      NIL)))
         (if method
             (remove-method #',name method)
             NIL)))))

(defmacro define-unbound-reader (class method &body default)
  (destructuring-bind (method slot) (enlist method method)
    `(defmethod ,method ((,class ,class))
       (cond ((slot-boundp ,class ',slot)
              (slot-value ,class ',slot))
             (T
              ,@default)))))

(defun class-default-initargs (class-ish)
  (let ((class (etypecase class-ish
                 (symbol (find-class class-ish))
                 (standard-class class-ish))))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    (c2mop:class-default-initargs class)))

(defmethod copy-instance ((instance standard-object) &key deep)
  (let ((copy (allocate-instance (class-of instance))))
    (loop for slot in (c2mop:class-slots (class-of instance))
          for name = (c2mop:slot-definition-name slot)
          for value = (slot-value instance name)
          do (setf (slot-value copy name) (if deep (copy-instance value) value)))
    copy))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kw (thing)
    (intern (string-upcase thing) "KEYWORD"))

  (defun mksym (package &rest parts)
    (let ((*print-case* (readtable-case *readtable*)))
      (intern (format NIL "~{~a~}" parts) package))))

(defun %lispify-name (name)
  (with-output-to-string (out)
    (loop with dash = T
          for char across name
          do (cond ((or #+sbcl (sb-unicode:whitespace-p char)
                        #-sbcl (find char '(#\Space #\Tab #\Linefeed #\Return))
                        (find char "-_,'`\"#;"))
                    (unless dash
                      (setf dash T)
                      (write-char #\- out)))
                   (T
                    (write-char (char-upcase char) out)
                    (setf dash NIL))))))

(defun lispify-name (name &optional (package "KEYWORD"))
  (etypecase name
    (integer name)
    (symbol name)
    (cons (cons (lispify-name (car name) package)
                (lispify-name (cdr name) package)))
    (string (intern (%lispify-name name) package))))

(defun enlist (item &rest items)
  (if (listp item) item (list* item items)))

(defun unlist (item)
  (if (listp item) (first item) item))

(defun unquote (item)
  (if (and (listp item) (eql 'quote (first item))) (second item) item))

(defun remf* (list &rest keys)
  (loop for (k v) on list by #'cddr
        for x = (member k keys)
        unless x collect k
        unless x collect v))

(defmacro popf (list key)
  (let ((value (gensym "VALUE"))
        (ksym (gensym "KEY")))
    `(let* ((,ksym ,key)
            (,value (getf ,list ,ksym)))
       (remf ,list ,ksym)
       ,value)))

(defun getf* (key list &key (test #'eql) default)
  (loop for (k v) on list by #'cddr
        do (when (funcall test key k) (return v))
        finally (return default)))

(defun remove-all (elements sequence &rest args)
  (apply #'remove-if (lambda (e) (member e elements)) sequence args))

(defun f32-vec (&rest args)
  (let ((array (make-array (length args) :element-type 'single-float)))
    (map-into array #'float args)))

(defun u32-vec (&rest args)
  (let ((array (make-array (length args) :element-type '(unsigned-byte 32))))
    (map-into array #'truncate args)))

(defun i32-vec (&rest args)
  (let ((array (make-array (length args) :element-type '(signed-byte 32))))
    (map-into array #'truncate args)))

(defmacro xor (a &rest options)
  (cond ((null options) a)
        (T
         (let ((found (gensym "FOUND")))
           `(let ((,found NIL))
              (block NIL
                ,@(loop for option in (list* a options)
                        collect `(when ,option
                                   (if ,found
                                       (return NIL)
                                       (setf ,found T))))
                ,found))))))

(defun one-of (thing &rest options)
  (find thing options))

(define-compiler-macro one-of (thing &rest options)
  (let ((thingg (gensym "THING")))
    `(let ((,thingg ,thing))
       (or ,@(loop for option in options
                   collect `(eql ,thingg ,option))))))

(defun input-source (&optional (stream *query-io*))
  (with-output-to-string (out)
    (loop for in = (read-line stream NIL NIL)
          while (and in (string/= in "EOF"))
          do (write-string in out))))

(defun input-value (&optional (stream *query-io*))
  (multiple-value-list (eval (read stream))))

(defun input-literal (&optional (stream *query-io*))
  (read stream))

(defmacro define-accessor-wrapper-methods (name &body wrappers)
  `(progn ,@(loop with value = (gensym "VALUE")
                  for (type resolution) in wrappers
                  collect `(defmethod ,name ((,type ,type))
                             (,name ,resolution))
                  collect `(defmethod (setf ,name) (,value (,type ,type))
                             (setf (,name ,resolution) ,value)))))

(defmacro define-accessor-delegate-methods (name &body wrappers)
  `(progn ,@(loop with value = (gensym "VALUE")
                  for (resolution type) in wrappers
                  collect `(defmethod ,name ((,type ,type))
                             (,resolution ,type))
                  collect `(defmethod (setf ,name) (,value (,type ,type))
                             (setf (,resolution ,type) ,value)))))

(defmacro with-retry-restart ((name report &rest report-args) &body body)
  (let ((tag (gensym "RETRY-TAG"))
        (return (gensym "RETURN"))
        (stream (gensym "STREAM")))
    `(block ,return
       (tagbody
          ,tag (restart-case
                   (return-from ,return
                     (progn ,@body))
                 (,name ()
                   :report (lambda (,stream) (format ,stream ,report ,@report-args))
                   (go ,tag)))))))

(defmacro with-new-value-restart ((place &optional (input 'input-value))
                                  (name report &rest report-args) &body body)
  (let ((tag (gensym "RETRY-TAG"))
        (return (gensym "RETURN"))
        (stream (gensym "STREAM"))
        (value (gensym "VALUE")))
    `(block ,return
       (tagbody
          ,tag (restart-case
                   (return-from ,return
                     (progn ,@body))
                 (,name (,value)
                   :report (lambda (,stream) (format ,stream ,report ,@report-args))
                   :interactive ,input
                   (setf ,place ,value)
                   (go ,tag)))))))

(defmacro with-unwind-protection (cleanup &body body)
  `(unwind-protect
        (progn ,@body)
     ,cleanup))

(defmacro with-cleanup-on-failure (cleanup-form &body body)
  (let ((success (gensym "SUCCESS")))
    `(let ((,success NIL))
       (unwind-protect
            (multiple-value-prog1
                (progn
                  ,@body)
              (setf ,success T))
         (unless ,success
           ,cleanup-form)))))

(defun constantly-restart (restart &rest values)
  (lambda (&rest args)
    (declare (ignore args))
    (apply #'invoke-restart restart values)))

(defmacro with-accessors* (accessors instance &body body)
  `(with-accessors ,(loop for accessor in accessors
                          collect (enlist accessor accessor))
       ,instance
     ,@body))

(defmacro with-accessor-values (bindings instance &body body)
  `(let ,(loop for binding in bindings
               for (var acc) = (enlist binding binding)
               collect `(,var (,acc ,instance)))
     ,@body))

(defun acquire-lock-with-starvation-test (lock &key (warn-time 10) timeout)
  (assert (or (null timeout) (< warn-time timeout)))
  (flet ((do-warn () (v:warn :trial.core "Failed to acquire ~a for ~s seconds. Possible starvation!"
                             lock warn-time)))
    #+sbcl (or (sb-thread:grab-mutex lock :timeout warn-time)
               (do-warn)
               (if timeout
                   (sb-thread:grab-mutex lock :timeout (- timeout warn-time))
                   (sb-thread:grab-mutex lock)))
    #-sbcl (loop with start = (get-universal-time)
                 for time = (- (get-universal-time) start)
                 thereis (bt:acquire-lock lock NIL)
                 do (when (and warn-time (< warn-time time))
                      (setf warn-time NIL)
                      (do-warn))
                    (when (and timeout (< timeout time))
                      (return NIL))
                    (bt:thread-yield))))

(defmacro with-trial-io-syntax ((&optional (package '*package*)) &body body)
  (let ((pkg (gensym "PACKAGE")))
    `(let ((,pkg (etypecase ,package
                   ((or string symbol) (find-package ,package))
                   (package ,package))))
       (with-standard-io-syntax
         (let ((*package* ,pkg)
               (*print-case* :downcase)
               (*print-readably* NIL))
           ,@body)))))

(defun parse-sexps (input)
  (with-trial-io-syntax ()
    (etypecase input
      (string
       (loop with i = 0
             collect (multiple-value-bind (data next) (read-from-string input NIL #1='#:EOF :start i)
                       (setf i next)
                       (if (eql data #1#)
                           (loop-finish)
                           data))))
      (stream
       (loop for value = (read input NIL #1#)
             until (eq value #1#)
             collect value)))))

(defun envvar-directory (var)
  (let ((var (uiop:getenv var)))
    (when (and var (string/= "" var))
      (pathname-utils:parse-native-namestring var :as :directory :junk-allowed T))))

(defun tempdir ()
  (or #+windows (or (envvar-directory "TEMP") #p"~/AppData/Local/Temp/")
      #+darwin (envvar-directory "TMPDIR")
      #+linux (envvar-directory "XDG_RUNTIME_DIR")
      #p"/tmp/"))

(defun tempfile (&key (id (format NIL "trial-~a-~a" (get-universal-time) (random 1000)))
                      (type "tmp"))
  (make-pathname :name id
                 :type type
                 :defaults (tempdir)))

(defmacro with-tempfile ((path &rest args) &body body)
  `(let ((,path (tempfile ,@args)))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,path)
         (delete-file ,path)))))

(defun make-uuid (&optional (id NIL id-p))
  (let ((val (random #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))
        (id (cond (id-p id)
                  (+main+ (sxhash (username +main+))))))
    (format NIL "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
            (ldb (byte 32  0) (or id val))
            (ldb (byte 16 32) val)
            (ldb (byte 16 48) val)
            (ldb (byte 16 64) val)
            (ldb (byte 48 80) val))))

(defun logfile ()
  (let ((log (or (uiop:getenv "TRIAL_LOGFILE") "")))
    (merge-pathnames (if (string= "" log)
                         "trial.log"
                         (pathname-utils:parse-native-namestring log))
                     (or (uiop:argv0) (user-homedir-pathname)))))

(defun config-directory (&rest app-path)
  (apply #'pathname-utils:subdirectory
         (or (envvar-directory "TRIAL_CONFIG_HOME")
             #+windows
             (or (envvar-directory "AppData")
                 (pathname-utils:subdirectory (user-homedir-pathname) "AppData" "Roaming"))
             (or (envvar-directory "XDG_CONFIG_HOME")
                 (pathname-utils:subdirectory (user-homedir-pathname) ".config")))
         (or app-path (list +app-vendor+ +app-system+))))

(defun standalone-logging-handler ()
  (when (deploy:deployed-p)
    (when (logfile)
      (ignore-errors (delete-file (logfile)))
      (v:define-pipe ()
          (v:file-faucet :file (logfile))))
    (v:info :trial "Running on ~a, ~a ~a, ~a ~a"
            (machine-instance) (machine-type) (machine-version)
            (software-type) (software-version))))

(defun make-thread (name func)
  (bt:make-thread (lambda ()
                    (handler-bind ((error #'standalone-error-handler))
                      (funcall func)))
                  :name name
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (*error-output* . ,*error-output*)
                                      (*trace-output* . ,*trace-output*)
                                      (*standard-input* . ,*standard-input*)
                                      (*query-io* . ,*query-io*)
                                      (*debug-io* . ,*debug-io*)
                                      (*package* . ,*package*))))

(defmacro with-thread ((name) &body body)
  `(make-thread ,name (lambda () ,@body)))

(defun wait-for-thread-exit (thread &key (timeout 1) (interval 0.1))
  (loop for i from 0
        while (bt:thread-alive-p thread)
        do (sleep interval)
           (when (= i (/ timeout interval))
             (restart-case
                 (error 'thread-did-not-exit :thread thread :timeout (* i interval))
               (continue ()
                 :report "Continue waiting.")
               (debug ()
                 :report "Try to interrupt the thread with a break."
                 (bt:interrupt-thread thread (lambda () (break))))
               (abort ()
                 :report "Kill the thread and exit, risking corrupting the image."
                 (bt:destroy-thread thread)
                 (return))))))

(defmacro with-thread-exit ((thread &key (timeout 1) (interval 0.1)) &body body)
  (let ((thread-g (gensym "THREAD")))
    `(let ((,thread-g ,thread))
       (when (and ,thread-g (bt:thread-alive-p ,thread-g))
         ,@body
         (wait-for-thread-exit ,thread-g :timeout ,timeout :interval ,interval)))))

(defmacro with-error-logging ((&optional (category :trial) (message "") &rest args) &body body)
  (let ((category-g (gensym "CATEGORY")))
    `(let ((,category-g ,category))
       (handler-bind ((error (lambda (err)
                               (v:severe ,category-g "~@[~@? ~]~a" ,message ,@args err)
                               (v:debug ,category-g err))))
         ,@body))))

(defmacro with-ignored-errors-on-release ((&optional (category :trial) (message "") &rest args) &body body)
  (declare (ignorable category message args))
  #+trial-release
  `(ignore-errors
    (with-error-logging (,category ,message ,@args)
      ,@body))
  #-trial-release
  `(with-simple-restart (continue "Ignore the error and continue.")
     ,@body))

(defmacro with-timing-report ((level category &optional (format "Operation took ~fs run-time ~fs real-time") &rest args) &body body)
  (let ((run (gensym "RUNTIME"))
        (real (gensym "REALTIME")))
    `(let ((,run (get-internal-run-time))
           (,real (get-internal-real-time)))
       (unwind-protect
            (progn ,@body)
         (v:log ,(kw level) ,category ,format ,@args
                (/ (- (get-internal-run-time) ,run) INTERNAL-TIME-UNITS-PER-SECOND)
                (/ (- (get-internal-real-time) ,real) INTERNAL-TIME-UNITS-PER-SECOND))))))

(defun format-timestring (&key (timestamp (get-universal-time)) (as :datetime))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time timestamp)
    (ecase as
      (:filename (format NIL "~4,'0d-~2,'0d-~2,'0d ~2,'0d-~2,'0d-~2,'0d" yy mm dd h m s))
      (:datetime (format NIL "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s))
      (:date (format NIL "~4,'0d-~2,'0d-~2,'0d" yy mm dd))
      (:time (format NIL "~2,'0d:~2,'0d:~2,'0d" h m s))
      (:clock (format NIL "~2,'0d:~2,'0d" h m)))))

(defgeneric descriptor (object))

(defmethod descriptor (object)
  (format NIL "~a@~4,'0x" (type-of object)
          #-sbcl (sxhash object)
          #+sbcl (sb-kernel:get-lisp-obj-address object)))

(defmethod descriptor ((number number))
  (format NIL "~a" number))

(defmethod descriptor ((string string))
  (format NIL "~s" string))

(defun simplify (array &optional (element-type (array-element-type array)))
  (if (and (typep array 'simple-array)
           (equal element-type (array-element-type array)))
      array
      (make-array (length array)
                  :element-type element-type
                  :initial-contents array)))

(defun ensure-instance (object type &rest initargs)
  (cond ((null object)
         (apply #'make-instance type initargs))
        ((eql (type-of object) type)
         (apply #'reinitialize-instance object initargs))
        (T
         (apply #'change-class object type initargs))))

(defun ensure-class (class-ish)
  (etypecase class-ish
    (symbol (find-class class-ish))
    (standard-class class-ish)
    (standard-object (class-of class-ish))))

(defun type-prototype (type)
  (case type
    (character #\Nul)
    (complex #c(0 0))
    (cons '(NIL . NIL))
    (float 0.0)
    (function #'identity)
    (hash-table (load-time-value (make-hash-table)))
    (integer 0)
    (null NIL)
    (package #.*package*)
    (pathname #p"")
    (random-state (load-time-value (make-random-state)))
    (readtable (load-time-value (copy-readtable)))
    (stream (load-time-value (make-broadcast-stream)))
    (string "string")
    (symbol 'symbol)
    (vector #(vector))
    (T (let ((class (find-class type)))
         (unless (c2mop:class-finalized-p class)
           (c2mop:finalize-inheritance class))
         (c2mop:class-prototype class)))))

(defun list-eql-specializers (function &rest args)
  (delete-duplicates
   (loop for method in (c2mop:generic-function-methods function)
         for spec = (loop for arg in args
                          collect (nth arg (c2mop:method-specializers method)))
         when (loop for arg in spec
                    thereis (typep arg 'c2mop:eql-specializer))
         collect (loop for arg in spec
                       collect (if (typep arg 'c2mop:eql-specializer) (c2mop:eql-specializer-object arg) arg)))))

(defun maybe-finalize-inheritance (class)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class)))))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    class))

(defun list-subclasses (class)
  (let ((sub (c2mop:class-direct-subclasses (ensure-class class))))
    (loop for class in sub
          nconc (list* class (list-subclasses class)))))

(defun list-leaf-classes (root)
  (let ((sub (c2mop:class-direct-subclasses (ensure-class root))))
    (if sub
        (remove-duplicates
         (loop for class in sub
               nconc (list-leaf-classes class)))
        (list (ensure-class root)))))

(defmacro with-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-direct-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defmacro with-all-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defmethod find-slot ((name symbol) (class class))
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (find name (c2mop:class-slots class)))

(defmethod find-slot (name (object standard-object))
  (find-slot name (class-of object)))

(defmethod construct-delegate-object-type (delegate base &rest args)
  (apply #'make-instance delegate args))

(defmacro define-constant-fold-function (name (arg) &body body)
  (let ((whole (gensym "WHOLE"))
        (env (gensym "ENV"))
        (thunk (mksym (symbol-package name) "%" name)))
    `(progn
       (defun ,thunk (,arg)
         ,@body)
       (setf (fdefinition ',name) #',thunk)
       (define-compiler-macro ,name (&whole ,whole &environment ,env ,arg)
         (if (constantp ,arg ,env)
             `(load-time-value (,',thunk ,,arg))
             ,whole)))))

(defgeneric clone (thing &key &allow-other-keys))
(defgeneric <- (target source)
  (:method-combination progn :most-specific-last))
(defgeneric initargs (thing)
  (:method-combination append :most-specific-last))

(defmethod initargs append (thing) ())
(defmethod clone (thing &key) thing)
(defmethod <- progn (a b))

(defmethod clone ((vec vec2) &key) (vcopy vec))
(defmethod <- progn ((target vec2) (source vec2)) (v<- target source))
(defmethod clone ((vec vec3) &key) (vcopy vec))
(defmethod <- progn ((target vec3) (source vec3)) (v<- target source))
(defmethod clone ((vec vec4) &key) (vcopy vec))
(defmethod <- progn ((target vec4) (source vec4)) (v<- target source))
(defmethod clone ((mat mat2) &key) (mcopy mat))
(defmethod <- progn ((target mat2) (source mat2)) (m<- target source))
(defmethod clone ((mat mat3) &key) (mcopy mat))
(defmethod <- progn ((target mat3) (source mat3)) (m<- target source))
(defmethod clone ((mat mat4) &key) (mcopy mat))
(defmethod <- progn ((target mat4) (source mat4)) (m<- target source))
(defmethod clone ((mat matn) &key) (mcopy mat))
(defmethod <- progn ((target matn) (source matn)) (m<- target source))
(defmethod clone ((quat quat) &key) (qcopy quat))
(defmethod <- progn ((target quat) (source quat)) (q<- target source))

(defmethod clone ((cons cons) &key)
  (cons (clone (car cons)) (clone (cdr cons))))

(defmethod clone ((array array) &key)
  (if (array-has-fill-pointer-p array)
      (make-array (array-dimensions array)
                  :element-type (array-element-type array)
                  :adjustable (adjustable-array-p array)
                  :fill-pointer (fill-pointer array)
                  :initial-contents array)
      (make-array (array-dimensions array)
                  :element-type (array-element-type array)
                  :adjustable (adjustable-array-p array)
                  :initial-contents array)))

(defmethod clone ((object standard-object) &rest initargs)
  (<- (apply #'make-instance (class-of object) initargs) object))

(defmacro define-transfer (class &body properties)
  `(defmethod <- progn ((target ,class) (source ,class))
     ,@(loop for property in properties
             collect (if (and (listp property) (eql :eval (first property)))
                         `(progn ,@(rest property))
                         (destructuring-bind (target-accessor &optional (source-accessor target-accessor) (key 'identity)) (enlist property)
                           `(setf (,target-accessor target) (,key (,source-accessor source))))))
     target))

(defmethod location ((vec vec2)) vec)

(defmethod location ((vec vec3)) vec)

(defmethod location ((mat mat3))
  (with-fast-matref (m mat)
    (vec (m 0 2) (m 1 2))))

(defmethod location ((mat mat4))
  (with-fast-matref (m mat)
    (vec (m 0 3) (m 1 3) (m 2 3))))

(defmethod orientation ((mat mat4))
  (qfrom-mat mat))

(defmethod global-location ((vec vec2) &optional (target (vec3)))
  (let ((vec (vec (vx vec) (vy vec) 0 0)))
    (declare (dynamic-extent vec))
    (n*m (model-matrix) vec)
    (vsetf target (vx vec) (vy vec) (vz vec))))

(defmethod global-location ((vec vec3) &optional (target (vec3)))
  (let ((vec (vec (vx vec) (vy vec) (vz vec) 0)))
    (declare (dynamic-extent vec))
    (n*m (model-matrix) vec)
    (vsetf target (vx vec) (vy vec) (vz vec))))

(defun initarg-slot (class initarg)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class)))))
    (find (list initarg) (c2mop:class-slots class)
          :key #'c2mop:slot-definition-initargs
          :test #'subsetp)))

(defun initarg-slot-value (instance initarg)
  (slot-value instance (c2mop:slot-definition-name (initarg-slot (class-of instance) initarg))))

(defun minimize (sequence test &key (key #'identity))
  (etypecase sequence
    (vector (when (< 0 (length sequence))
              (loop with minimal = (aref sequence 0)
                    for i from 1 below (length sequence)
                    for current = (aref sequence i)
                    do (when (funcall test
                                      (funcall key current)
                                      (funcall key minimal))
                         (setf minimal current))
                    finally (return minimal))))
    (list (when sequence
            (loop with minimal = (car sequence)
                  for current in (rest sequence)
                  do (when (funcall test
                                    (funcall key current)
                                    (funcall key minimal))
                       (setf minimal current))
                  finally (return minimal))))))

(defun format-with-line-numbers (text &optional out)
  (etypecase out
    (null
     (with-output-to-string (out)
       (format-with-line-numbers text out)))
    ((eql T)
     (format-with-line-numbers text *standard-output*))
    (stream
     (with-input-from-string (in text)
       (loop for i from 1
             for line = (read-line in NIL)
             while line
             do (format out "~3d " i)
                (write-line line out))))))

(defun generate-name (&optional indicator)
  (loop for name = (format NIL "~a-~d" (or indicator "ENTITY") (incf *gensym-counter*))
        while (find-symbol name *package*)
        finally (return (intern name *package*))))

(declaim (inline clamp))
(defun clamp (low mid high)
  (max low (min mid high)))

(declaim (inline deadzone))
(defun deadzone (min thing)
  (if (< (abs thing) min) 0.0 thing))

(declaim (inline lpf))
(defun lpf (factor cur target)
  (+ (* (- 1.0 factor) target) (* cur factor)))

(declaim (inline lerp))
(defun lerp (from to n)
  (etypecase from
    (real (+ (* from (- 1.0 n)) (* to n)))
    (vec (vlerp from to n))))

(declaim (inline lerp-dt))
(defun lerp-dt (from to dt rem)
  ;; REM: remaining time to get to TO.
  (declare (optimize speed))
  (let* ((l (- (/ (float rem 0f0)
                  (float (log 1/100 2) 0f0))))
         (n (- 1 (expt 2 (- (/ (float dt 0f0) l))))))
    (declare (type single-float n l))
    (lerp from to n)))

(declaim (inline deg->rad rad->deg))
(defun deg->rad (deg)
  (* deg PI 1/180))

(defun rad->deg (rad)
  (* rad 180 (/ PI)))

(defun db (db)
  (expt 10 (* 0.05 db)))

(defun angle-midpoint (a b)
  (when (< b a) (rotatef a b))
  (when (< PI (- b a)) (decf b F-2PI))
  (mod (/ (+ b a) 2) F-2PI))

(defun angle-distance (a b)
  (let ((da (mod (- b a) F-2PI)))
    (- (mod (* 2 da) F-2PI) da)))

(defun clamp-angle (min a max)
  (flet ((normalize-180 (a)
           (- (mod (+ a F-PI) F-2PI) F-PI)))
    (let* ((a (mod a F-2PI))
           (n-min (normalize-180 (- min a)))
           (n-max (normalize-180 (- max a))))
      (cond ((and (<= n-min 0) (<= 0 n-max))
             a)
            ((< (abs n-min) (abs n-max))
             min)
            (T
             max)))))

(defun lerp-angle (a b x)
  (let ((d (- b a)))
    (cond ((< (+ F-PI) d) (incf a F-2PI))
          ((< d (- F-PI)) (decf a F-2PI)))
    (+ a (* (- b a) x))))

#-3d-math-u32
(progn
  (declaim (inline uvarr2 uvarr3 uvarr4 uvec2 uvec3 uvec4))
  (defun uvarr2 (a) (ivarr2 a))
  (defun uvarr3 (a) (ivarr3 a))
  (defun uvarr4 (a) (ivarr4 a))
  (defun uvec2 (&rest args) (apply #'ivec2 args))
  (defun uvec3 (&rest args) (apply #'ivec3 args))
  (defun uvec4 (&rest args) (apply #'ivec4 args)))

(defparameter *c-chars* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_[]")

(defun symbol->c-name (symbol &optional out)
  (flet ((frob (out)
           (loop for c across (symbol-name symbol)
                 do (cond ((char= c #\-)
                           (write-char #\_ out))
                          ((find c *c-chars*)
                           (write-char (char-downcase c) out))
                          (T (write-char #\_ out))))))
    (if out
        (frob out)
        (with-output-to-string (out)
          (frob out)))))

(defun c-name->symbol (name &optional (package *package*))
  (intern
   (with-output-to-string (out)
     (loop for c across name
           do (cond ((char= c #\_)
                     (write-char #\- out))
                    (T
                     (write-char (char-upcase c) out)))))
   package))

(defun gl-vendor ()
  (let ((vendor (gl:get-string :vendor)))
    (cond ((search "Intel" vendor) :intel)
          ((search "NVIDIA" vendor) :nvidia)
          ((search "ATI" vendor) :amd)
          ((search "AMD" vendor) :amd)
          (T :unknown))))

(defun check-texture-size (width height)
  (let ((max (gl:get* :max-texture-size)))
    (when (< max (max width height))
      (error "Hardware cannot support a texture of size ~ax~a, max is ~a."
             width height max))))

(defmacro define-enum-check (name &body cases)
  (let* ((*print-case* (readtable-case *readtable*))
         (list (mksym *package* "*" name "-" '#:list "*"))
         (func (mksym *package* '#:check "-" name))
         (funcp (mksym *package* name '#:-p)))
    `(progn (defparameter ,list '(,@cases))
            (defun ,func (enum)
              (unless (member enum ,list)
                (error "~a is not a valid ~a. Needs to be one of the following:~%~a"
                       enum ',name ,list)))
            (defun ,funcp (enum)
              (member enum ,list)))))

(define-enum-check texture-target
  :texture-1d :texture-2d :texture-3d
  :texture-1d-array :texture-2d-array
  :texture-cube-map :texture-cube-map-array
  :texture-2d-multisample :texture-2d-multisample-array)

(define-enum-check texture-mag-filter
  :nearest :linear)

(define-enum-check texture-min-filter
  :nearest :linear :nearest-mipmap-nearest :nearest-mipmap-linear
  :linear-mipmap-nearest :linear-mipmap-linear)

(define-enum-check texture-wrapping
  :repeat :mirrored-repeat :clamp-to-edge :clamp-to-border)

(define-enum-check texture-internal-format
  :red :r8 :r8-snorm :r8i :r8ui
  :r16 :r16-snorm :r16f :r16i :r16ui
  :r32f :r32i :r32ui
  :rg :rg8 :rg8-snorm :rg8i :rg8ui
  :rg16 :rg16-snorm :rg16f :rg16i :rg16ui
  :rg32f :rg32i :rg32ui
  :rgb :rgb8 :rgb8-snorm :rgb8i :rgb8ui
  :r3-g3-b2 :rgb4 :rgb5 :rgb9-e5 :rgb10 :r11f-g11f-b10f :rgb12
  :rgb16-snorm :rgb16f :rgb16i :rgb16ui
  :rgb32f :rgb32i :rgb32ui
  :rgba :rgba2 :rgba4 :rgb5-a1 :rgb10-a2 :rgb10-a2ui :rgba12
  :rgba8 :rgba8-snorm :rgba8i :rgba8ui
  :rgba16 :rgba16f :rgba16i :rgba16ui
  :rgba32f :rgba32i :rgba32ui
  :srgb :srgb-alpha :srgb8 :srgb8-alpha8
  :depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f
  :stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16
  :depth-stencil :depth24-stencil8 :depth32f-stencil8
  :compressed-red :compressed-red-rgtc1 :compressed-signed-red-rgtc1
  :compressed-rg :compressed-rg-rgtc2 :compressed-signed-rg-rgtc2
  :compressed-rgb :compressed-rgb-bptc-signed-float :compressed-rgb-bptc-unsigned-float
  :compressed-rgba :compressed-rgba-bptc-unorm
  :compressed-srgb :compressed-srgb-alpha :compressed-srgb-alpha-bptc-unorm)

(define-enum-check texture-pixel-format
  :red :rg :rgb :bgr :rgba :bgra
  :red-integer :rg-integer :rgb-integer
  :bgr-integer :rgba-integer :bgra-integer
  :stencil-index :depth-component :depth-stencil)

(define-enum-check texture-pixel-type
  :unsigned-byte :byte
  :unsigned-short :short
  :unsigned-int :int
  :half-float :float
  :unsigned-byte-3-3-2 :unsigned-byte-2-3-3-rev
  :unsigned-short-5-6-5 :unsigned-short-5-6-5-rev
  :unsigned-short-4-4-4-4 :unsigned-short-4-4-4-4-rev
  :unsigned-short-5-5-5-1 :unsigned-short-1-5-5-5-rev
  :unsigned-int-8-8-8-8 :unsigned-int-8-8-8-8-rev
  :unsigned-int-10-10-10-2 :unsigned-int-2-10-10-10-rev
  :unsigned-int-24-8 :float-32-unsigned-int-24-8-rev
  :unsigned-int-5-9-9-9-rev)

(define-enum-check shader-type
  :compute-shader :vertex-shader
  :geometry-shader :fragment-shader
  :tess-control-shader :tess-evaluation-shader)

(define-enum-check vertex-buffer-element-type
  :byte :unsigned-byte :short :unsigned-short :int :unsigned-int
  :half-float :float :double :fixed)

(define-enum-check buffer-object-type
  :array-buffer :atomic-counter-buffer
  :copy-read-buffer :copy-write-buffer
  :dispatch-indirect-buffer :draw-indirect-buffer
  :element-array-buffer :pixel-pack-buffer
  :pixel-unpack-buffer :query-buffer
  :shader-storage-buffer :texture-buffer
  :transform-feedback-buffer :uniform-buffer)

(define-enum-check buffer-object-data-usage
  :stream-draw :stream-read :stream-copy :static-draw
  :static-read :static-copy :dynamic-draw :dynamic-read
  :dynamic-copy)

(define-enum-check framebuffer-attachment
  :color-attachment0 :color-attachment1 :color-attachment2 :color-attachment3
  :color-attachment4 :color-attachment5 :color-attachment6 :color-attachment7
  :depth-attachment :stencil-attachment :depth-stencil-attachment)

(define-enum-check sampler-type
  :sampler-1d
  :sampler-2d
  :sampler-3d
  :sampler-cube
  :sampler-1d-shadow
  :sampler-2d-shadow
  :sampler-1d-array
  :sampler-2d-array
  :sampler-1d-array-shadow
  :sampler-2d-array-shadow
  :sampler-2d-multisample
  :sampler-2d-multisample-array
  :sampler-cube-shadow
  :sampler-buffer
  :sampler-2d-rect
  :sampler-2d-rect-shadow
  :int-sampler-1d
  :int-sampler-2d
  :int-sampler-3d
  :int-sampler-cube
  :int-sampler-1d-array
  :int-sampler-2d-array
  :int-sampler-2d-multisample
  :int-sampler-2d-multisample-array
  :int-sampler-buffer
  :int-sampler-2d-rect
  :unsigned-int-sampler-1d
  :unsigned-int-sampler-2d
  :unsigned-int-sampler-3d
  :unsigned-int-sampler-cube
  :unsigned-int-sampler-1d-array
  :unsigned-int-sampler-2d-array
  :unsigned-int-sampler-2d-multisample
  :unsigned-int-sampler-2d-multisample-array
  :unsigned-int-sampler-buffer
  :unsigned-int-sampler-2d-rect
  :image-1d
  :image-2d
  :image-3d
  :image-2d-rect
  :image-cube
  :image-buffer
  :image-1d-array
  :image-2d-array
  :image-2d-multisample
  :image-2d-multisample-array
  :int-image-1d
  :int-image-2d
  :int-image-3d
  :int-image-2d-rect
  :int-image-cube
  :int-image-buffer
  :int-image-1d-array
  :int-image-2d-array
  :int-image-2d-multisample
  :int-image-2d-multisample-array
  :unsigned-int-image-1d
  :unsigned-int-image-2d
  :unsigned-int-image-3d
  :unsigned-int-image-2d-rect
  :unsigned-int-image-cube
  :unsigned-int-image-buffer
  :unsigned-int-image-1d-array
  :unsigned-int-image-2d-array
  :unsigned-int-image-2d-multisample
  :unsigned-int-image-2d-multisample-array)

(defun internal-format-components (format)
  (ecase format
    ((:red :r8 :r8-snorm :r8i :r8ui
      :r16 :r16-snorm :r16f :r16i :r16ui
      :r32f :r32i :r32ui
      :compressed-red :compressed-red-rgtc1
      :compressed-signed-red-rgtc1) 1)
    ((:rg :rg8 :rg8-snorm :rg8i :rg8ui :gr
      :rg16 :rg16-snorm :rg16f :rg16i :rg16ui
      :compressed-rg :compressed-rg-rgtc2 :compressed-signed-rg-rgtc2) 2)
    ((:rg32f :rg32i :rg32ui :bgr
      :rgb :rgb8 :rgb8-snorm :rgb8i :rgb8ui
      :r3-g3-b2 :rgb4 :rgb5 :rgb9-e5 :rgb10 :r11f-g11f-b10f :rgb12
      :rgb16-snorm :rgb16f :rgb16i :rgb16ui
      :rgb32f :rgb32i :rgb32ui :srgb8
      :compressed-rgb :compressed-rgb-bptc-signed-float
      :compressed-rgb-bptc-unsigned-float :compressed-srgb) 3)
    ((:rgba :bgra :rgba2 :rgba4 :rgb5-a1 :rgb10-a2 :rgb10-a2ui :rgba12
      :rgba8 :rgba8-snorm :rgba8i :rgba8ui
      :rgba16 :rgba16f :rgba16i :rgba16ui
      :rgba32f :rgba32i :rgba32ui :srgb8-alpha8
      :compressed-rgba :compressed-rgba-bptc-unorm
      :compressed-srgb-alpha :compressed-srgb-alpha-bptc-unorm) 4)
    ((:depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f
      :stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16) 1)
    ((:depth-stencil :depth24-stencil8 :depth32f-stencil8) 2)))

(defun internal-format-pixel-format (format)
  (ecase format
    ((:stencil-index :stencil-index1 :stencil-index4 :stencil-index8 :stencil-index16)
     :stencil-index)
    ((:depth-component :depth-component16 :depth-component24 :depth-component32 :depth-component32f)
     :depth-component)
    ((:depth-stencil :depth24-stencil8 :depth32f-stencil8)
     :depth-stencil)
    ((:r8i :r16i :r32i :r8ui :r16ui :r32ui)
     :red-integer)
    ((:rg8i :rg16i :rg32i :rg8ui :rg16ui :rg32ui)
     :rg-integer)
    ((:gr8i :gr16i :gr32i :gr8ui :gr16ui :gr32ui)
     :gr-integer)
    ((:rgb8i :rgb16i :rgb32i :rgb8ui :rgb16ui :rgb32ui)
     :rgb-integer)
    ((:bgr8i :bgr16i :bgr32i :bgr8ui :bgr16ui :bgr32ui)
     :bgr-integer)
    ((:rgba8i :rgba16i :rgba32i :rgba8ui :rgba16ui :rgba32ui)
     :rgba-integer)
    ((:bgra8i :bgra16i :bgra32i :bgra8ui :bgra16ui :bgra32ui)
     :bgra-integer)
    ((:red :r8 :r16 :r32 :r16f :r32f)
     :red)
    ((:rg :rg8 :rg16 :rg32 :rg16f :rg32f)
     :rg)
    ((:gr :gr8 :gr16 :gr32 :gr16f :gr32f)
     :gr)
    ((:rgb :rgb8 :rgb16 :rgb32 :rgb16f :rgb32f :rgb9-e5)
     :rgb)
    ((:bgr :bgr8 :bgr16 :bgr32 :bgr16f :bgr32f)
     :bgr)
    ((:rgba :rgba8 :rgba16 :rgba32 :rgba16f :rgba32f)
     :rgba)
    ((:bgra :bgra8 :bgra16 :bgra32 :bgra16f :bgra32f)
     :bgra)))

(defun internal-format-pixel-type (format)
  (ecase format
    ((:red :r8 :r8-norm :rg :rg8 :rg8-snorm :rgb :rgb8 :rgb8-snorm :rgba :rgba8 :rgba8-snorm
      :r8ui :rg8ui :rgb8ui :rgba8ui :depth-component :stencil-index :stencil-index1 :stencil-index
      :stencil-index8)
     :unsigned-byte)
    ((:r16 :r16-snorm :rg16 :rg16-snorm :rgb16 :rgb16-snorm :rgba16 :rgba16-snorm
      :r16ui :rg16ui :rgb16ui :rgba16ui :depth-component16 :stencil-index16)
     :unsigned-short)
    ((:r32 :r32-snorm :rg32 :rg32-snorm :rgb32 :rgb32-snorm :rgba32 :rgba32-snorm
      :r32ui :rg32ui :rgb32ui :rgba32ui :depth-component24 :depth-component32)
     :unsigned-int)
    ((:r8i :rg8i :rgb8i :rgba8i)
     :byte)
    ((:r16i :rg16i :rgb16i :rgba16i)
     :short)
    ((:r32i :rg32i :rgb32i :rgba32i)
     :int)
    ((:r16f :rg16f :rgb16f :rgba16f)
     :half-float)
    ((:r32f :rg32f :rgb32f :rgba32f :depth-component32f)
     :float)
    (:depth-stencil :depth24-stencil8
     :unsigned-int-24-8)
    (:depth32f-stencil8
     :float-32-unsigned-int-24-8-rev)
    ((:rgb9-e5)
     :unsigned-int-5-9-9-9-rev)))

(defun pixel-data-stride (pixel-type pixel-format)
  (* (ecase pixel-format
       ((:r :red :red-integer :stencil-index :depth-component :depth-stencil)
        1)
       ((:rg :gr :rg-integer)
        2)
       ((:rgb :bgr :rgb-integer :bgr-integer)
        3)
       ((:rgba :bgra :rgba-integer :bgra-integer)
        4))
     (ecase pixel-type
       ((:byte :unsigned-byte)
        1)
       ((:unsigned-byte-3-3-2 :unsigned-byte-2-3-3-rev)
        1/3)
       ((:short :unsigned-short :short-float)
        2)
       ((:unsigned-short-5-6-5 :unsigned-short-5-6-5-rev
         :unsigned-short-5-5-5-1 :unsigned-short-1-5-5-5-rev)
        2/3)
       ((:unsigned-short-4-4-4-4 :unsigned-short-4-4-4-4-rev)
        2/4)
       ((:int :unsigned-int :float 
              :unsigned-int-24-8 :unsigned-int-8-24-rev-mesa :float-32-unsigned-int-24-8-rev)
        4)
       ((:unsigned-int-5-9-9-9-rev :unsigned-int-10-10-10-2 :unsigned-int-2-10-10-10-rev)
        4/3)
       ((:unsigned-int-8-8-8-8 :unsigned-int-8-8-8-8-rev)
        4/4))))

(defun infer-internal-format (pixel-type pixel-format)
  (mksym
   "KEYWORD"
   (ecase pixel-format
     ((:r :red) :r)
     ((:rg :gr) :rg)
     ((:rgb :bgr) :rgb)
     ((:rgba :bgra) :rgba))
   (ecase pixel-type
     ((:byte :unsigned-byte) :8)
     ((:short :unsigned-short) :16)
     ((:int :unsigned-int) :32)
     ((:short-float) :16f)
     ((:float) :32f)
     (:unsigned-int-5-9-9-9-rev :9-e5))))

(defun infer-swizzle-format (pixel-format)
  (ecase pixel-format
    ((:r :red) '(:r :r :r :r))
    ((:rg :gr) '(:r :r :r :g))
    ((:rgb :bgr) '(:r :g :b 1))
    ((:rgba :bgra) '(:r :g :b :a))))

(defun infer-swizzle-channels (channels)
  (flet ((f (&rest fields)
           (loop for c in channels thereis (find c fields))))
    (let ((r (f :r :red)) (g (f :g :green)) (b (f :b :blue)) (a (f :a :alpha)))
      (cond ((and r (not g) (not b) (not a)) (list :r :r :r :r))
            ((and (not r) g (not b) (not a)) (list :g :g :g :g))
            ((and (not r) (not g) b (not a)) (list :b :b :b :b))
            ((and r (not g) (not b) a) (list :r :r :r :a))
            ((and (not r) g (not b) a) (list :g :g :g :a))
            ((and (not r) (not g) b a) (list :b :b :b :a))
            (T (list (cond (r :r) (g  1) (b  1) (a :a))
                     (cond (g :g) (b  1) (r  1) (a :a))
                     (cond (b :b) (g  1) (r  1) (a :a))
                     (cond (a :a) (T  1))))))))

(defun infer-pixel-type (depth type)
  (ecase depth
    ((1 8) (ecase type
             (:signed :byte)
             (:unsigned :unsigned-byte)))
    (16 (ecase type
          (:signed :short)
          (:unsigned :unsigned-short)
          (:float :half-float)))
    (32 (ecase type
          (:signed :int)
          (:unsigned :unsigned-int)
          (:float :float)))))

(defun pixel-type->cl-type (type)
  (case type
    (:byte '(signed-byte 8))
    (:unsigned-byte '(unsigned-byte 8))
    (:short '(signed-byte 16))
    (:unsigned-short '(unsigned-byte 16))
    (:int '(signed-byte 32))
    (:unsigned-int '(unsigned-byte 32))
    (:half-float 'short-float)
    (:float 'single-float)
    (:double 'double-float)
    (T T)))

(defun cl-type->pixel-type (type)
  (typecase type
    (cons
     (destructuring-bind (form arity) type
       (ecase form
         (signed-byte
          (ecase arity
            (8 :byte)
            (16 :short)
            (32 :int)))
         (unsigned-byte
          (ecase arity
            (8 :unsigned-byte)
            (16 :unsigned-short)
            (32 :unsigned-int))))))
    (symbol
     (ecase type
       (single-float :float)
       (double-flot :double)))))

(define-global +gl-extensions+ ())

(defmacro %cache (value)
  (let ((cache (gensym "CACHE")))
    `(let ((,cache (load-time-value (cons NIL NIL))))
       (or (car ,cache) (setf (car ,cache) ,value)))))

(defun cache-gl-extensions ()
  (let ((*package* (find-package "KEYWORD")))
    (setf +gl-extensions+
          (loop for i from 0 below (gl:get* :num-extensions)
                for name = (ignore-errors (gl:get-string-i :extensions i))
                when name
                collect (cffi:translate-name-from-foreign name *package*)))))

(defun gl-extension-p (extension)
  (find extension +gl-extensions+))

(define-compiler-macro gl-extension-p (extension)
  `(%cache (find ,extension +gl-extensions+)))

(defmacro when-gl-extension (extension &body body)
  (let ((list (enlist extension)))
    `(when (%cache (and ,@(loop for extension in list
                                collect `(find ,extension +gl-extensions+))))
       ,@body)))

(defmacro gl-extension-case (&body cases)
  `(cond ,@(loop for (extensions . body) in cases
                 collect (case extensions
                           ((T otherwise)
                            `(T ,@body))
                           (T
                            `((%cache (and ,@(loop for extension in (enlist extensions)
                                                   collect `(find ,extension +gl-extensions+))))
                              ,@body))))))

(declaim (inline dbg))
#-trial-release
(defun dbg (&rest parts)
  (let ((*print-right-margin* 10000000000))
    (format *debug-io* "~&~{~a~^ ~}~%" parts)))

#+trial-release
(defun dbg (&rest parts)
  (declare (ignore parts)))

(defun %adjust-array (array length &optional (constructor (constantly NIL)))
  (let* ((old (length array)))
    (unless (= old length)
      (setf array (adjust-array array length))
      (loop for i from old below length
            do (setf (aref array i) (funcall constructor))))
    array))

(defun find-program (program)
  (loop for dir in (uiop:getenv-absolute-directories "PATH")
        thereis (when dir (probe-file (merge-pathnames program dir)))))

(defun run (program &rest args)
  (flet ((normalize (arg)
           (etypecase arg
             (pathname (pathname-utils:native-namestring arg))
             (real (princ-to-string arg))
             (string arg))))
    (let ((args (loop for arg in args
                      append (mapcar #'normalize (enlist arg))))
          (program (or #+windows (find-program (format NIL "~a.exe" program))
                       (find-program program)
                       (error "Can't find external program:~%  ~a"
                              program))))
      (uiop:run-program (list* program args)
                        :output :string :error-output *error-output*))))
