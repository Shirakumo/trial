(defpackage #:org.shirakumo.fraf.trial.selftest
  (:use #:cl+trial)
  (:export
   #:*test-output*
   #:run
   #:run-test))
(in-package #:org.shirakumo.fraf.trial.selftest)

(defvar *test-output* *standard-output*)
(defvar *tests* (make-array 0 :adjustable T :fill-pointer T))
(defvar *failures*)

(defun start-test (i name)
  (format *test-output* "~&[~3d/~3d] ~a ~32t"
          i (length *tests*) name)
  (finish-output *test-output*))

(defun finish-test (name result)
  (typecase result
    (condition
     (push name *failures*)
     (format *test-output* "FAILED (~a)~%" (type-of result)))
    (T
     (format *test-output* "~@<~@;~a~;~:>~%" result)))
  (finish-output *test-output*))

(defun funcall-muffled (fn)
  (let* ((*standard-output* (make-broadcast-stream))
         (*error-output* *standard-output*)
         (*query-io* *standard-output*))
    (handler-case
        (handler-bind ((warning #'muffle-warning))
          (funcall fn))
      (error (e) e))))

(defun run-test (name &key muffle)
  (let ((fn (second (or (find name *tests* :key #'first :test #'string-equal)
                        (error "No such test ~a" name)))))
    (if muffle
        (funcall-muffled fn)
        (funcall fn))))

(defun run (&key skip)
  (let ((*failures* ()))
    (org.shirakumo.verbose:with-muffled-logging ()
      (loop for (test fn) across *tests*
            for i from 1
            do (cond ((null fn)
                      (format *standard-output* "~&~% == ~a ==~%" test))
                     ((find test skip :test #'string-equal)
                      (start-test i test)
                      (finish-test test :SKIPPED))
                     (T
                      (start-test i test)
                      (finish-test test (run-test-muffled fn))))))
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

(defun remove-test (name)
  (array-utils:vector-pop-position
   *tests* (or (position name *tests* :key #'first :test #'string-equal)
               (error "No such test ~s" name))))

(define-simple-save-file v0 :latest
  (:decode (depot))
  (:encode (depot)))

(defclass dummy ()
  ((context :initform (make-context NIL :visible NIL) :accessor context)
   (thunk :initarg :thunk :initform (constantly NIL) :accessor thunk)))

(defmethod start ((dummy dummy))
  (with-context ((context dummy))
    (funcall (thunk dummy)))
  (quit (context dummy)))

(defmacro context-test (name &body body)
  `(test ,name
     (let ((context (make-context NIL :visible NIL)))
       (with-unwind-protection (finalize context)
         (create-context context)
         (with-context (context)
           ,@body)))))

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
(test "User home dir" (user-homedir-pathname))
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
(test "Trial pool path" (base (find-pool 'trial)))
(test "Allocate memory" (deallocate (allocate (make-instance 'memory :size 64))))
(test "Load cat" (load-image (input* (asset 'trial 'trial::cat)) T))

(group "Context")
(test "Create context" (finalize (create-context (make-context NIL :visible NIL))))
(test "Make current" (let ((context (create-context (make-context NIL :visible NIL))))
                       (make-current context)
                       (finalize context)))
(test "Launch with context" (launch-with-context 'dummy))
(context-test "GL Info"
  (context-info *context* :stream NIL))
(context-test "Swap buffers"
  (show *context*)
  (gl:clear-color 0 1 0 1)
  (gl:clear :color-buffer-bit)
  (swap-buffers *context*)
  (sleep 0.2))
(context-test "List monitors"
  (mapcar #'list-video-modes (list-monitors *context*)))
(context-test "Fullscreen"
  (show *context* :fullscreen T))
(context-test "Allocate shader"
  (allocate (make-instance 'shader :type :fragment-shader :source "void main(){}")))
(context-test "Allocate texture"
  (allocate (make-instance 'texture :width 1024 :height 1024)))
(context-test "Allocate framebuffer"
  (let ((tex (make-instance 'texture :width 1 :height 1)))
    (allocate tex)
    (allocate (make-instance 'framebuffer :attachments `((:color-attachment0 ,tex))))))
(context-test "Allocate buffer"
  (allocate (make-instance 'vertex-buffer :buffer-data (trial::f32-vec 0 0 0))))
(context-test "Primitive render"
  (let* ((vao (// 'trial 'fullscreen-square))
         (vs (make-instance 'shader :type :vertex-shader :source "
layout (location = 0) in vec3 position;
void main(){ gl_Position = vec4(position, 1.0f); }"))
         (fs (make-instance 'shader :type :fragment-shader :source "
out vec4 color;
void main(){ color = vec4(0,1,0,1); }"))
         (prog (make-instance 'shader-program :shaders (list vs fs))))
    (with-unwind-protection (deallocate (asset 'trial 'fullscreen-square))
      (show *context*)
      (activate (trial::ensure-allocated prog))
      (render (trial::ensure-allocated vao) T)
      (swap-buffers *context*)
      (sleep 0.2))))
