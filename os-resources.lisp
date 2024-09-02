(in-package #:org.shirakumo.fraf.trial)

#+windows
(cffi:define-foreign-library secur32
    (T (:default "Secur32")))

(defun system-username ()
  (or #+windows
      (cffi:with-foreign-objects ((size :ulong)
                                  (name :uint16 128))
        (unless (cffi:foreign-library-loaded-p 'secur32)
          (cffi:load-foreign-library 'secur32))
        (setf (cffi:mem-ref size :ulong) 128)
        ;; Constant 3 here specifies a "display name".
        (cond ((< 0 (cffi:foreign-funcall "GetUserNameExW" :int 13 :pointer name :pointer size :int))
               (org.shirakumo.com-on:wstring->string name (cffi:mem-ref size :ulong)))
              (T
               (setf (cffi:mem-ref size :ulong) 128)
               (when (< 0 (cffi:foreign-funcall "GetUserNameW" :pointer name :pointer size :int))
                 (org.shirakumo.com-on:wstring->string name (cffi:mem-ref size :ulong))))))
      #+nx
      (cffi:with-foreign-object (nick :char 33)
        (when (cffi:foreign-funcall "nxgl_username" :pointer nick :int 33 :bool)
          (cffi:foreign-string-to-lisp nick :max-chars 33)))
      #+unix
      (cffi:foreign-funcall "getlogin" :string)
      (pathname-utils:directory-name (user-homedir-pathname))))

(defvar *open-in-browser-hook* (constantly NIL))
(defun open-in-browser (url)
  (or (funcall *open-in-browser-hook* url)
      (org.shirakumo.open-with:open url)))

(defun open-in-file-manager (path)
  (org.shirakumo.open-with:open (pathname path)))

(defun rename-thread (name)
  (ignore-errors
   #+windows
   (com:with-wstring (name name)
     (cffi:foreign-funcall "SetThreadDescription"
                           :size (cffi:foreign-funcall "GetCurrentThread" :size)
                           :string name
                           :size))
   #+nx
   (cffi:foreign-funcall "nxgl_set_thread_name"
                         :pointer (cffi:null-pointer)
                         :string name
                         :int)
   #+unix
   (cffi:foreign-funcall "pthread_setname_np"
                         :size (cffi:foreign-funcall "pthread_self" :size)
                         :string name
                         :int)))

(macrolet ((define-wrap (name fun)
             `(progn
                (defun ,name ()
                  (,fun))

                (trivial-deprecate:declaim-deprecated (function ,name)
                                                      :software "trial"
                                                      :version "1.2.0"
                                                      :alternatives (,fun)))))

  (define-wrap io-bytes org.shirakumo.machine-state:process-io-bytes)

  (define-wrap cpu-time org.shirakumo.machine-state:process-time)

  (define-wrap cpu-room org.shirakumo.machine-state:gc-room)

  (define-wrap gpu-room org.shirakumo.machine-state:gpu-room)

  (define-wrap gpu-time org.shirakumo.machine-state:gpu-time)

  (define-wrap gc-time org.shirakumo.machine-state:gc-time))
