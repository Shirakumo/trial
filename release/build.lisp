(in-package #:org.shirakumo.fraf.trial.release)

(defvar *default-build-features*
  '(:trial-optimize-all :cl-opengl-no-masked-traps :cl-opengl-no-check-error
    :cl-mixed-no-restarts :trial-release))

(defgeneric build (target &key))
(defgeneric test (target &key))

(defmethod build :around (target &key &allow-other-keys)
  (restart-case
      (call-next-method)
    (continue ()
      :report "Treat the build as successful")
    (retry ()
      :report "Retry the build"
      (build target))))

(defun build-args (&key (features (append *default-build-features* (config :build :features)))
                        (build-arguments (config :build :build-arguments))
                        (dynamic-space-size (config :build :dynamic-space-size))
                        (force T))
  (append (list "--dynamic-space-size" (princ-to-string dynamic-space-size)
                "--eval" "(setf asdf:*user-cache* (asdf::xdg-cache-home \"common-lisp\" \"trial-release\":implementation))"
                "--eval" "(asdf:initialize-output-translations)"
                "--eval" (format NIL "(setf *features* (append *features* '~s))" features))
          build-arguments
          (list "--eval" (format NIL "(asdf:make ~s :force ~s)" (config :system) force)
                "--disable-debugger" "--quit")))

(defmethod build ((target (eql :linux)) &rest args &key &allow-other-keys)
  #+linux (apply #'run (config :build :linux) (apply #'build-args args))
  #+windows (apply #'run "wsl.exe" (config :build :linux) (build-args)))

(defmethod build ((target (eql :windows)) &rest args &key &allow-other-keys)
  (apply #'run (config :build :windows) (apply #'build-args args)))

(defmethod build ((target (eql :macos)) &rest args &key &allow-other-keys)
  (apply #'run (config :build :macos) (apply #'build-args args)))

(defmethod build ((target (eql T)) &rest args &key &allow-other-keys)
  (apply #'build (config :build :targets) args))

(defmethod build ((targets list) &rest args &key &allow-other-keys)
  (dolist (target targets)
    (apply #'build target args)))

(defmethod test :around (target &key &allow-other-keys)
  (setf (uiop:getenv "TRIAL_QUIT_AFTER_INIT") "true")
  (unwind-protect
       (restart-case (call-next-method)
         (continue ()
           :report "Treat test as successful")
         (retry ()
           :report "Retry the test"
           (test target))
         (rebuild ()
           :report "Rebuild and retry"
           (build target)
           (test target)))
    (setf (uiop:getenv "TRIAL_QUIT_AFTER_INIT") "")))

(defmethod test ((target (eql :linux)) &key)
  (dolist (file (directory (merge-pathnames "bin/*.run" (asdf:system-source-directory (config :system)))))
    #+linux (run file)
    #+windows (run "wsl.exe" file)))

(defmethod test ((target (eql :windows)) &key)
  (dolist (file (directory (merge-pathnames "bin/*.exe" (asdf:system-source-directory (config :system)))))
    #+windows (run file)
    #-windows (run "wine" file)))

(defmethod test ((target (eql T)) &rest args &key &allow-other-keys)
  (apply #'test (config :build :targets) args))

(defmethod test ((targets list) &rest args &key &allow-other-keys)
  (dolist (target targets)
    (apply #'test target args)))
