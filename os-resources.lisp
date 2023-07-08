#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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

(defun cpu-room ()
  #+sbcl
  (values (round (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage)) 1024.0)
          (round (sb-ext:dynamic-space-size) 1024.0))
  #-sbcl (values 1 1))
