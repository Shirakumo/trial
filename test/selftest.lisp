(defpackage #:org.shirakumo.fraf.trial.selftest
  (:use #:cl+trial)
  (:export #:run))
(in-package #:org.shirakumo.fraf.trial.selftest)

(defparameter *tests* (make-array 0 :adjustable T :fill-pointer T))
(defvar *failures*)

(defun start-test (i name)
  (format *standard-output* "~&[~3d/~3d] ~a ~30t"
          i (length *tests*) name))

(defun finish-test (name result)
  (typecase result
    (condition
     (push name *failures*)
     (format *standard-output* "FAILED (~a)~%" (type-of result)))
    (T
     (format *standard-output* "~a~%" result))))

(defun run-test-fn (fn)
  (let* ((*standard-output* (make-broadcast-stream))
         (*error-output* *standard-output*)
         (*query-io* *standard-output*))
    (handler-case
        (handler-bind ((warning #'muffle-warning))
          (funcall fn))
      (error (e) e))))

(defun run ()
  (let ((*failures* ()))
    (org.shirakumo.verbose:with-muffled-logging ()
      (loop for (test fn) across *tests*
            for i from 1
            do (cond (fn
                      (start-test i test)
                      (finish-test test (run-test-fn fn)))
                     (T
                      (format *standard-output* "~&~% == ~a ==~%" test)))))
    (format *standard-output* "~&~%~:[All OK!~;~:*Some failures occurred:~{~%  ~a~}~]~%" *failures*)))

(defmacro test (name &body body)
  (let ((fn (trial:lispify-name (format NIL "test/~a" name) *package*)))
    `(let* ((name ,name)
            (idx (or (position name *tests* :key #'first :test #'string-equal)
                     (vector-push-extend (list name NIL) *tests*))))
       (flet ((,fn () ,@body))
         (setf (second (aref *tests* idx)) #',fn)))))

(defmacro group (name)
  `(let* ((name ,name)
          (idx (or (position name *tests* :key #'first :test #'string-equal)
                   (vector-push-extend (list name NIL) *tests*))))
     (setf (second (aref *tests* idx)) NIL)))

(define-simple-save-file v0 :latest
  (:decode (depot))
  (:encode (depot)))

(group "Basic Lisp information")
(test "Machine type" (machine-type))
(test "Machine version" (machine-version))
(test "Software type" (software-type))
(test "Software version" (software-version))
(test "Lisp implementation type" (lisp-implementation-type))
(test "Lisp implementation version" (lisp-implementation-version))

(group "Threading")
(test "Create thread" (wait-for-thread-exit (with-thread ("Test"))))
(test "Rename thread" (rename-thread "TEST"))

(group "GC")
(test "Run GC" (trivial-garbage:gc))
(test "Run full GC" (trivial-garbage:gc :full T))

(group "Query machine information")
(test "CPU time" (cpu-time))
(test "CPU room" (cpu-room))
(test "GC time" (gc-time))
(test "IO bytes" (io-bytes))

(group "Query user information")
(test "Username" (system-username))
(test "Language" (system-locale:language))

(group "Launch external programs")
(test "Open browser" (open-in-browser "https://shirakumo.org"))
(test "File manager" (open-in-file-manager (self)))
(test "Error message" (emessage "Test"))

(group "Runtime environment tests")
(test "Self" (self))
(test "Checksum" (trial::checksum (self)))
(test "Data root" (data-root))
(test "Version" (version :trial))
(test "Precise time" (current-time))
(test "Tempdir" (tempdir))
(test "Create tempfile" (with-tempfile (path) (alexandria:write-string-into-file "test" path)))
(test "Logfile" (logfile))
(test "Create logfile" (alexandria:write-string-into-file "test" (logfile) :if-exists :supersede))
(test "Config directory" (config-directory))
(test "Save settings" (progn (save-settings) T))

(group "Powersaving")
(test "Prevent powersaving" (trial::prevent-powersave))
(test "Restore powersaving" (trial::restore-powersave))

(group "Save files")
(test "Create save file" (store-save-data 1 T))
(test "Load save file" (load-save-data 1 T))
(test "List save files" (list-save-files))
(test "Delete save files" (delete-save-files))

(group "Gamepad querying")
(test "List gamepads" (org.shirakumo.fraf.gamepad:init))
(test "Poll gamepads" (org.shirakumo.fraf.gamepad:poll-devices :timeout NIL))
(test "Rumble" (let ((dev (first (org.shirakumo.fraf.gamepad:list-devices))))
                 (when dev
                   (org.shirakumo.fraf.gamepad:rumble dev 1.0)
                   (sleep 0.5)
                   (org.shirakumo.fraf.gamepad:rumble dev 0.0))))

(group "Audio")
(test "Platform drain" (org.shirakumo.fraf.harmony:detect-platform-drain))
(test "Initialize output" (let* ((packer (org.shirakumo.fraf.mixed:make-packer))
                                 (drain (make-instance (org.shirakumo.fraf.harmony:detect-platform-drain)
                                                       :pack (org.shirakumo.fraf.mixed:pack packer))))
                            (org.shirakumo.fraf.mixed:start drain)
                            (org.shirakumo.fraf.mixed:end drain)
                            (org.shirakumo.fraf.mixed:free drain)))
(test "Create simple server" (let ((server (org.shirakumo.fraf.harmony:make-simple-server)))
                               (org.shirakumo.fraf.mixed:start server)
                               (org.shirakumo.fraf.mixed:free server)))

(group "Asset loading")

(group "Context")
