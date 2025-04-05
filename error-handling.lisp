(in-package #:org.shirakumo.fraf.trial)

(defvar *inhibit-standalone-error-handler* NIL)
(defvar *error-report-hook* #'identity)

(defun standard-error-hook (err)
  (declare (ignore err))
  (org.shirakumo.messagebox:show
   (format NIL "An unhandled error occurred. Please send the application logfile to the developers. You can find it here:~%~%~a"
           (pathname-utils:native-namestring (trial:logfile)))
   :title (format NIL "Failed to run ~a" +app-system+)
   :type :error
   :modal T)
  (deploy:quit))

(setf *error-report-hook* #'standard-error-hook)

(defun emessage (string &rest args)
  (org.shirakumo.messagebox:show (format NIL "~?" string args)
                                 :title (format NIL "Failed to run ~a" +app-system+)
                                 :type :error
                                 :modal T)
  :exit)

(defgeneric report-on-error (error))

(defmethod report-on-error ((error condition))
  (funcall *error-report-hook* error))

(defmethod report-on-error ((error file-error))
  (emessage "Failed to access the file ~a. The file does not exist, cannot be accessed, or is corrupted." (file-error-pathname error)))

(defmethod report-on-error ((error storage-condition))
  (emessage "~a ran out of memory and cannot continue, sorry." +app-system+))

#+sbcl
(defmethod report-on-error ((error sb-sys:memory-fault-error))
  (emessage "~a encountered a memory corruption and cannot continue, sorry." +app-system+))

(defmethod report-on-error ((error trial:thread-did-not-exit))
  :exit)

(defmethod report-on-error ((error trial:context-creation-error))
  (emessage "Failed to initialise OpenGL. This either means your system is not capable of running ~a, or your driver is currently bugged.~@[The following error was generated:~% ~a~]"
            +app-system+ (slot-value error 'message)))

(defmethod report-on-error ((error trial:shader-compilation-error))
  (emessage "Failed to compile shaders. Please ensure your graphics card and drivers are up-to-date enough to run ~a." +app-system+))

(defmethod report-on-error ((error gl:opengl-error))
  (case (cdr (%gl::opengl-error.error-code error))
    (:out-of-memory
     (emessage "Ran out of graphics memory. Your graphics card's or your computer's memory may be too small to run ~a, sorry." +app-system+))
    (:invalid-operation
     ;; Ignoring this in the hopes it's a temporary issue that can just be ignored.
     :ignore)
    (T
     (funcall *error-report-hook* error))))

(defmethod report-on-error ((error org.shirakumo.depot:depot-condition))
  (emessage "A ~a resource file is corrupted and could not be read. Please verify your installation." +app-system+))

(defmethod report-on-error ((error org.shirakumo.zippy:zippy-condition))
  (emessage "A ~a resource file is corrupted and could not be read. Please verify your installation." +app-system+))

(defmethod report-on-error ((error cffi:load-foreign-library-error))
  (emessage "Failed to load a shared library (~a). Your system is either not capable of running ~a, or the shared library is corrupted."
            #+windows "DLL" #+darwin "DYLIB" #-(or windows darwin) "SO" +app-system+))

(defun standalone-error-handler (err &key (category :trial))
  (when (and (deploy:deployed-p) (not *inhibit-standalone-error-handler*))
    (v:error category err)
    (v:fatal category "Encountered unhandled error in ~a" (bt:current-thread))
    (cond ((deploy:env-set-p "DEPLOY_CONTINUE_ERROR")
           (if (find-restart 'continue)
               (continue err)
               (abort err)))
          ((deploy:env-set-p "DEPLOY_DEBUG_BOOT")
           #+sbcl (sb-ext:enable-debugger)
           (invoke-debugger err))
          (T
           (case (report-on-error err)
             (:exit (deploy:quit)))))))
