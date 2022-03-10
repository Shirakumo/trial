(in-package #:org.shirakumo.fraf.trial.release)

(defvar *default-build-features*
  '(:trial-optimize-all :cl-opengl-no-masked-traps :cl-opengl-no-check-error
    :cl-mixed-no-restarts))

(defmethod build :around (target)
  (with-simple-restart (continue "Treat build as successful")
    (call-next-method)))

(defun build-args ()
  (let ((features (append *default-build-features* (config :build :features))))
    (append (list "--dynamic-space-size" (princ-to-string (config :build :dynamic-space-size))
                  "--eval" (format NIL "(setf *features* (append *features* '~s))" features))
            (config :build :build-arguments)
            (list "--eval" (format NIL "(asdf:make :~a :force T)" (config :system))
                  "--disable-debugger" "--quit"))))

(defmethod build ((target (eql :linux)))
  (apply #'run (config :build :linux) (build-args)))

(defmethod build ((target (eql :windows)))
  #-windows (apply #'run (config :build :windows) (build-args))
  #+windows (apply #'run "sbcl.exe" (build-args)))

(defmethod build ((target (eql :macos)))
  #-darwin (apply #'run (config :build :macos) (build-args))
  #+darwin (apply #'run "sbcl" (build-args)))

(defmethod build ((target (eql T)))
  (dolist (target (config :build :targets))
    (build target)))

(defmethod build ((targets cons))
  (dolist (target targets)
    (build target)))

(defmethod build ((target null)))
