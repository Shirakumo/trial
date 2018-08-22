#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun mmap-protection-flag (flag)
  (ecase flag
    (:none osicat-posix:prot-none)
    (:read osicat-posix:prot-read)
    (:write osicat-posix:prot-write)
    (:exec osicat-posix:prot-exec)))

(defun mmap-flag (flag)
  (ecase flag
    (:shared osicat-posix:map-shared)
    (:private osicat-posix:map-private)
    (:fixed osicat-posix:map-fixed)
    (:failed osicat-posix:map-failed)
    (:no-reserve osicat-posix:map-noreserve)
    (:locked osicat-posix:map-locked)
    (:grows-down osicat-posix:map-growsdown)
    (:anonymous osicat-posix:map-anonymous)
    (:populate osicat-posix:map-populate)
    (:non-block osicat-posix:map-nonblock)))

(defun fopen-flag (flag)
  (ecase flag
    (:read osicat-posix:o-rdonly)
    (:write osicat-posix:o-wronly)
    (:create osicat-posix:o-creat)
    (:exclusive osicat-posix:o-excl)
    (:truncate osicat-posix:o-trunc)
    (:append osicat-posix:o-append)
    (:no-c-tty osicat-posix:o-noctty)
    (:non-block osicat-posix:o-nonblock)
    (:ndleay osicat-posix:o-ndelay)
    (:sync osicat-posix:o-sync)
    (:no-follow osicat-posix:o-nofollow)
    (:async osicat-posix:o-async)
    (:direct osicat-posix:o-direct)
    (:directory osicat-posix:o-directory)
    (:large-file osicat-posix:o-largefile)
    (:d-sync osicat-posix:o-dsync)
    (:r-sync osicat-posix:o-rsync)))

(declaim (inline %mmap))
(defun %mmap (path open-flags protection-flags map-flags)
  (declare (type fixnum open-flags protection-flags map-flags))
  (declare (type string path))
  (declare (optimize speed))
  #+unix
  (let ((fd (osicat-posix:open path open-flags)))
    (unwind-protect
         (let* ((size (osicat-posix:stat-size (osicat-posix:fstat fd)))
                (addr (osicat-posix:mmap (cffi:null-pointer) size
                                         protection-flags
                                         map-flags
                                         fd 0)))
           (values addr size))
      (osicat-posix:close fd)))
  ;; FIXME: This https://msdn.microsoft.com/en-us/library/aa366761.aspx
  #+windows
  (error "Implement MMAP compatibility layer on Windows you fuck")
  #-(or windows unix)
  (error "What kind of system even is this?"))

(defun mmap (path &key (open '(:read)) (protection '(:read)) (mmap '(:private)))
  (%mmap (etypecase path
           (string path)
           (pathname (uiop:native-namestring path)))
         (reduce #'logior open :key #'fopen-flag)
         (reduce #'logior protection :key #'mmap-protection-flag)
         (reduce #'logior mmap :key #'mmap-flag)))

(define-compiler-macro mmap (&environment env path &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)))
  (flet ((constant-fold (var form)
           (if (constantp var env)
               `(load-time-value ,form)
               form)))
    `(%mmap ,(constant-fold path `(etypecase ,path
                                    (string ,path)
                                    (pathname (uiop:native-namestring ,path))))
            ,(constant-fold open `(reduce #'logior ,open :key #'fopen-flag))
            ,(constant-fold protection `(reduce #'logior ,protection :key #'mmap-protection-flag))
            ,(constant-fold mmap `(reduce #'logior ,mmap :key #'mmap-flag)))))

(declaim (inline munmap))
(defun munmap (addr size)
  #+unix
  (osicat-posix:munmap addr size)
  #+windows
  (error "Implement MMAP compatibility layer on Windows you fuck")
  #-(or windows unix)
  (error "What kind of system even is this?"))

(defmacro with-mmap ((addr size path &rest args) &body body)
  `(multiple-value-bind (,addr ,size) (mmap ,path ,@args)
     (unwind-protect
          (progn ,@body)
       (munmap ,addr ,size))))
