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
      #+unix
      (cffi:foreign-funcall "getlogin" :string)
      (pathname-utils:directory-name (user-homedir-pathname))))

(defvar *open-in-browser-hook* (constantly NIL))
(defun open-in-browser (url)
  (or (funcall *open-in-browser-hook* url)
      #+windows
      (uiop:launch-program (list "rundll32" "url.dll,FileProtocolHandler" url))
      #+linux
      (uiop:launch-program (list "xdg-open" url))
      #+darwin
      (uiop:launch-program (list "open" url))))

(defun open-in-file-manager (path)
  #+windows
  (uiop:launch-program (list "explorer.exe" (uiop:native-namestring path)))
  #+linux
  (uiop:launch-program (list "xdg-open" (uiop:native-namestring path)))
  #+darwin
  (uiop:launch-program (list "open" (uiop:native-namestring path))))

(defun rename-thread (name)
  #+windows
  (com:with-wstring (name name)
    (ignore-errors
     (cffi:foreign-funcall "SetThreadDescription"
                           :size (cffi:foreign-funcall "GetCurrentThread" :size)
                           :string name
                           :size)))
  #+unix
  (ignore-errors
   (cffi:foreign-funcall "pthread_setname_np"
                         :size (cffi:foreign-funcall "pthread_self" :size)
                         :string name
                         :int)))

#+windows
(progn
  (cffi:defcstruct (io-counters :conc-name io-counters-)
    (reads :ullong)
    (writes :ullong)
    (others :ullong)
    (read-bytes :ullong)
    (write-bytes :ullong)
    (other-bytes :ullong))
  (cffi:defcfun (current-process "GetCurrentProcess") :pointer)
  (cffi:defcfun (process-io-counters "GetProcessIoCounters") :bool
    (process :pointer)
    (counters :pointer))

  (defun io-bytes ()
    (cffi:with-foreign-object (io-counters '(:struct io-counters))
      (process-io-counters (current-process) io-counters)
      (+ (io-counters-read-bytes io-counters)
         (io-counters-write-bytes io-counters)
         (io-counters-other-bytes io-counters))))

  (defun cpu-time ()
    0d0))

#+linux
(progn
  ;; FIXME: do this with netstat/taskstats at some point.
  (cffi:defcstruct (timeval :conc-name timeval-)
    (sec :uint64)
    (usec :uint64))
  (cffi:defcstruct (rusage :conc-name rusage-)
    (utime (:struct timeval))
    (stime (:struct timeval))
    (maxrss :long)
    (ixrss :long)
    (idrss :long)
    (isrss :long)
    (minflt :long)
    (majflt :long)
    (nswap :long)
    (inblock :long)
    (oublock :long)
    (msgsnd :long)
    (msgrcv :long)
    (nsignals :long)
    (nvcsw :long)
    (nivcsw :long))
  (cffi:defcfun (rusage "getrusage") :int
    (who :int)
    (struct :pointer))

  (defun io-bytes ()
    ;; KLUDGE: we do this in C to avoid the stream system overhead.
    (cffi:with-foreign-object (io :char 1024)
      (let ((file (cffi:foreign-funcall "fopen" :string "/proc/self/io" :string "rb" :pointer)))
        (cffi:foreign-funcall "fread" :pointer io :size 1 :size 1024 :pointer file :size)
        (cffi:foreign-funcall "fclose" :pointer file :void))
      (flet ((read-int (field)
               (let* ((start (cffi:foreign-funcall "strstr" :pointer io :string field :pointer))
                      (ptr (cffi:inc-pointer start (length field))))
                 (cffi:foreign-funcall "atoi" :pointer ptr :int))))
        (+ (read-int "read_bytes: ")
           (read-int "write_bytes: ")))))

  (defun cpu-time ()
    (cffi:with-foreign-object (rusage '(:struct rusage))
      (rusage 0 rusage)
      (+ (timeval-sec rusage)
         (/ (timeval-usec rusage) 1000000d0)))))

#-(or linux windows)
(defun io-bytes () 0)

#-(or linux windows)
(defun cpu-time () 0d0)

;; https://www.khronos.org/registry/OpenGL/extensions/ATI/ATI_meminfo.txt
(defun gpu-room-ati ()
  (let* ((vbo-free-memory-ati (gl:get-integer #x87FB 4))
         (tex-free-memory-ati (gl:get-integer #x87FC 4))
         (buf-free-memory-ati (gl:get-integer #x87FD 4))
         (total (+ (aref vbo-free-memory-ati 0)
                   (aref tex-free-memory-ati 0)
                   (aref buf-free-memory-ati 0))))
    (values total total)))

;; http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
(defun gpu-room-nvidia ()
  (let ((vidmem-total (gl:get-integer #x9047 1))
        (vidmem-free  (gl:get-integer #x9049 1)))
    (values vidmem-free
            vidmem-total)))

(defun gpu-room ()
  (macrolet ((jit (thing)
               `(ignore-errors
                 (return-from gpu-room
                   (multiple-value-prog1 ,thing
                     (compile 'gpu-room (lambda ()
                                          ,thing)))))))
    (jit (gpu-room-ati))
    (jit (gpu-room-nvidia))
    (jit (values 1 1))))

(defun cpu-room ()
  #+sbcl
  (values (round (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage)) 1024.0)
          (round (sb-ext:dynamic-space-size) 1024.0))
  #-sbcl (values 1 1))
