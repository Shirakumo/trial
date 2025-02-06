(defpackage #:org.shirakumo.fraf.trial.selftest
  (:use #:cl+trial)
  (:export
   #:*test-output*
   #:run
   #:run-test))
(in-package #:org.shirakumo.fraf.trial.selftest)

#-nx (defvar *test-output* *standard-output*)
#+nx (define-symbol-macro *test-output* *standard-output*)
(defvar *tests* (make-array 0 :adjustable T :fill-pointer T))
(defvar *failures*)
(defvar *default-skip* #-nx '()
        #+nx '(;; Not available
               "File manager"
               ;; Working but annoying
               "Open browser"
               "Error message"
               "Checksum"))

(defun start-test (name)
  (format *test-output* "~&[~3d/~3d] ~a ~32t"
          (position name *tests* :key #'car :test #'string-equal) (length *tests*) name)
  (finish-output *test-output*))

(defun finish-test (name result &rest args)
  (typecase result
    (condition
     (push (list* name result args) *failures*)
     (format *test-output* "FAILED (~a)~%" (type-of result)))
    (T
     (format *test-output* "~@<~@;~a~;~:>~%" result)))
  (finish-output *test-output*))

(defun funcall-muffled (fn &key (catch-errors T))
  (let* ((*standard-output* (make-broadcast-stream))
         (*error-output* *standard-output*)
         (*query-io* *standard-output*))
    (if catch-errors
        (handler-case
            (handler-bind ((warning #'muffle-warning))
              (funcall fn))
          (error (e) e))
        (handler-bind ((warning #'muffle-warning))
          (funcall fn)))))

(defun run-test (name &key muffle)
  (let ((fn (second (or (find name *tests* :key #'first :test #'string-equal)
                        (error "No such test ~a" name)))))
    (with-simple-restart (abort-test "Abort the test")
      (if muffle
          (funcall-muffled fn)
          (funcall fn)))))

(defun run (&key (skip *default-skip*))
  (let ((*failures* ()))
    (org.shirakumo.verbose:with-muffled-logging ()
      (loop for (test fn) across *tests*
            for i from 1
            do (cond ((null fn)
                      (format *test-output* "~&~% == ~a ==~%" test))
                     ((find test skip :test #'string-equal)
                      (start-test test)
                      (finish-test test :SKIPPED))
                     (T
                      (restart-case
                          (handler-bind ((error (lambda (e)
                                                  (invoke-restart 'abort-test e
                                                                  (with-output-to-string (out)
                                                                    (uiop:print-backtrace :stream out))))))
                            (start-test test)
                            (finish-test test (funcall-muffled fn :catch-errors NIL)))
                        (abort-test (e &rest args)
                          (apply #'finish-test test e args)))))))
    (cond ((null *failures*)
           (format *test-output* "~&~%All OK!~%"))
          (T
           (format *test-output* "~&~%Some failures occurred:~%")
           (loop for (test condition . args) in (nreverse *failures*)
                 do (start-test test)
                    (format *test-output* "~&~%~a~%[Condition of type ~s]~{~%~%~a~}~%"
                            condition (type-of condition) args))))))

(defmacro test (name &body body)
  (let ((fn (trial:lispify-name (format NIL "test/~a" name) *package*)))
    `(let* ((name ,name)
            (idx (or (position name *tests* :key #'first :test #'string-equal)
                     (vector-push-extend (list name NIL) *tests*))))
       (flet ((,fn () ,@body))
         (setf (second (aref *tests* idx)) #',fn)))))

(defmacro group (name &body body)
  `(progn (let* ((name ,name)
                 (idx (or (position name *tests* :key #'first :test #'string-equal)
                          (vector-push-extend (list name NIL) *tests*))))
            (setf (second (aref *tests* idx)) NIL))
          ,@body))

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

(defmethod finalize ((dummy dummy))
  (finalize (context dummy)))

(defmethod handle ((ev event) (handler (eql :dummy)))
  (org.shirakumo.verbose:info :trial.selftest "~a" ev))

(defmacro context-test (name &body body)
  `(test ,name
     (let ((context (make-context :dummy :visible NIL)))
       (with-unwind-protection (finalize context)
         (create-context context)
         (with-context (context)
           ,@body)))))

(defclass thunk-main (main)
  ((setup-thunk :initarg :setup-thunk :accessor setup-thunk)
   (duration :initarg :duration :initform 1.0 :accessor duration)))

(defmethod update :after ((main main) tt dt fc)
  (when (<= (duration main) tt)
    (quit (context main))))

(defmethod setup-scene ((main thunk-main) (scene scene))
  (funcall (setup-thunk main) scene))

(defmacro main-test (name (scene &rest initargs) &body body)
  `(test ,name
     (flet ((thunk (,scene) ,@body))
       (launch-with-context 'thunk-main :setup-thunk #'thunk ,@initargs))))

(define-shader-entity basic-triangle (listener vertex-entity transformed-entity vertex-colored-entity)
  ((vertex-array :initform (// 'trial 'triangle))))

(define-handler (basic-triangle tick) (dt)
  (vsetf (location basic-triangle) (* 0.5 (width *context*)) (* 0.5 (height *context*)) 0)
  (vsetf (scaling basic-triangle) (* 0.5 (width *context*)) (* 0.5 (height *context*)) 1)
  (trotate (tf basic-triangle) (qfrom-angle +vx+ (* 0.8 dt)))
  (trotate (tf basic-triangle) (qfrom-angle +vy+ (* 1 dt)))
  (trotate (tf basic-triangle) (qfrom-angle +vz+ (* 1.3 dt))))

(define-shader-entity basic-quad (vertex-entity transformed-entity colored-entity)
  ((vertex-array :initform (// 'trial 'unit-square))))

(defclass spawner (listener entity) ())

(define-handler (spawner tick) ()
  (dotimes (i 100)
    (enter (make-instance 'basic-quad :color (vec4 (random 1.0) (random 1.0) (random 1.0) 1.0)
                                      :location (vec3 (random 1280) (random 720) 1))
           (container spawner))))

(cffi:defcallback selftest :int ((in :int))
  (1+ in))

(group "Basic Lisp information"
  (test "Machine type" (machine-type))
  (test "Machine version" (machine-version))
  (test "Software type" (software-type))
  (test "Software version" (software-version))
  (test "Lisp implementation type" (lisp-implementation-type))
  (test "Lisp implementation version" (lisp-implementation-version)))

(group "FFI"
  (test "Heap alloc" (cffi:foreign-free (cffi:foreign-alloc :char :count 512)))
  (test "Stack alloc" (cffi:with-foreign-object (data :char 512)))
  (test "Call sin()" (cffi:foreign-funcall "sin" :double 1.0d0 :double))
  (test "Load libmixed" (cffi:load-foreign-library 'org.shirakumo.fraf.mixed.cffi:libmixed))
  (test "Lisp callback" (cffi:foreign-funcall-pointer (cffi:callback selftest) () :int 10 :int)))

(group "Internet"
  (test "TCP connect" (usocket:socket-close (usocket:socket-connect "example.com" 80)))
  (test "DNS query" (org.shirakumo.dns-client:query-data "example.com" :type :A)))

(group "Threading"
  (test "Create thread" (wait-for-thread-exit (with-thread ("Test"))))
  (test "Rename thread" (rename-thread "TEST")))

(group "GC"
  (test "Run GC" (trivial-garbage:gc))
  (test "Run full GC" (trivial-garbage:gc :full T))
  (test "Alloc 1GB in 1KB chunks"
    (dotimes (i (* 1024 1024))
      (make-array 1024 :element-type '(unsigned-byte 8))))
  (test "Alloc 1GB in 1MB chunks"
    (dotimes (i 1024)
      (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
  (test "Alloc 1GB in 1 chunk"
    (null (make-array (* 1024 1024 1024) :element-type '(unsigned-byte 8)))))

(group "Query machine information"
  (test "CPU time" (org.shirakumo.machine-state:process-time))
  (test "CPU room" (org.shirakumo.machine-state:gc-room))
  (test "GC time" (org.shirakumo.machine-state:gc-time))
  (test "IO bytes" (org.shirakumo.machine-state:process-io-bytes)))

(group "Query user information"
  (test "Username" (system-username))
  (test "Language" (system-locale:language)))

(group "Launch external programs"
  (test "Open browser" (open-in-browser "https://shirakumo.org"))
  (test "File manager" (open-in-file-manager (self)))
  (test "Error message" (emessage "Test")))

(group "Timing"
  (test "Time units/sec" INTERNAL-TIME-UNITS-PER-SECOND)
  (test "Internal real time" (get-internal-real-time))
  (test "Internal run time" (get-internal-run-time))
  (test "Universal time" (get-universal-time))
  (test "Date" (format-timestring))
  (test "Current time" (current-time))
  (test "Sleep 1s" (let ((start (current-time)))
                     (sleep 1)
                     (- (current-time) start))))

(group "Runtime environment tests"
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
  (test "Save settings" (save-settings) T))

(group "Powersaving"
  (test "Prevent powersaving" (prevent-powersave))
  (test "Ping powersaving" (ping-powersave 1.0))
  (test "Restore powersaving" (restore-powersave)))

(group "Save files"
  (test "Create save file" (store-save-data 1 T))
  (test "Load save file" (load-save-data 1 T))
  (test "List save files" (list-save-files))
  (test "Delete save files" (delete-save-files)))

(group "Gamepad querying"
  (test "List gamepads" (org.shirakumo.fraf.gamepad:init))
  (test "Poll gamepads" (org.shirakumo.fraf.gamepad:poll-devices :timeout NIL))
  (test "Rumble" (let ((dev (first (org.shirakumo.fraf.gamepad:list-devices))))
                   (when dev
                     (org.shirakumo.fraf.gamepad:rumble dev 1.0)
                     (sleep 0.5)
                     (org.shirakumo.fraf.gamepad:rumble dev 0.0)))))

(group "Audio"
  (test "Platform drain" (org.shirakumo.fraf.harmony:detect-platform-drain))
  (test "Initialize output" (let* ((packer (org.shirakumo.fraf.mixed:make-packer))
                                   (drain (make-instance (org.shirakumo.fraf.harmony:detect-platform-drain)
                                                         :pack (org.shirakumo.fraf.mixed:pack packer))))
                              (org.shirakumo.fraf.mixed:start drain)
                              (org.shirakumo.fraf.mixed:end drain)
                              (org.shirakumo.fraf.mixed:free drain)))
  (test "Create simple server" (let ((server (org.shirakumo.fraf.harmony:make-simple-server)))
                                 (org.shirakumo.fraf.mixed:start server)
                                 (org.shirakumo.fraf.mixed:free server))))

(group "Asset loading"
  (test "Trial pool path" (pool-path (find-pool 'trial) NIL))
  (test "Cat asset path" (input* (asset 'trial 'trial::cat)))
  (test "Allocate memory" (deallocate (allocate (make-instance 'memory :size 64))))
  (test "Load cat" (load-image (input* (asset 'trial 'trial::cat)) T)))

(group "Context"
  (test "Create context" (finalize (create-context (make-context NIL :visible NIL))))
  (test "Make current" (let ((context (create-context (make-context NIL :visible NIL))))
                         (make-current context)
                         (finalize context)))
  (context-test "Poll input"
    (poll-input *context*))
  (context-test "GL Info"
    (context-info *context* :stream NIL))
  (context-test "Swap buffers"
    (show *context*)
    (gl:clear-color 0 1 0 1)
    (gl:clear :color-buffer-bit)
    (swap-buffers *context*)
    (sleep 0.2))
  (context-test "Swap buffers threaded"
    (trial::release-context *context*)
    (let ((thread (with-thread ("render-thread")
                    (with-context (*context*)
                      (show *context*)
                      (gl:clear-color 0 1 0 1)
                      (gl:clear :color-buffer-bit)
                      (swap-buffers *context*)
                      (sleep 0.2)))))
      (wait-for-thread-exit thread)
      (trial::acquire-context *context*)))
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
  (test "Launch with context" (launch-with-context 'dummy))
  (main-test "Launch main" (scene :duration 0)
    (declare (ignore scene)))
  (main-test "Triangle" (scene)
    (nmortho (projection-matrix) 0 1280 0 720 0 1)
    (enter (make-instance 'basic-triangle) scene)
    (enter (make-instance 'render-pass) scene))
  (main-test "Debug text" (scene)
    (nmortho (projection-matrix) 0 1280 0 720 0 1)
    (enter (make-instance 'debug-text :text "HELLO" :foreground (vec4 1) :font-size 500) scene)
    (enter (make-instance 'render-pass) scene))
  (main-test "FPS counter" (scene)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'render-pass) scene))
  (main-test "Display controller" (scene)
    (enter (make-instance 'display-controller) scene)
    (enter (make-instance 'render-pass) scene)
    (observe! (length scene) :title "Entity count"))
  (main-test "Spawn" (scene)
    (nmortho (projection-matrix) 0 1280 0 720 0 1)
    (enter (make-instance 'spawner) scene)
    (preload 'basic-quad scene)
    (enter (make-instance 'display-controller) scene)
    (enter (make-instance 'render-pass) scene)
    (observe! (length scene) :title "Entity count")))
