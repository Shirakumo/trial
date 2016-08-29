#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-user-libs (libstem-gamepad cl-gamepad-cffi::*static*)
  (cl-gamepad-cffi:libstem-gamepad))

(define-user-libs (libmonitors cl-monitors-cffi::*static*)
  (cl-monitors-cffi:libmonitors))

(defun copy-dir (dir to)
  (let* ((dir (pathname-utils:to-directory dir))
         (base (pathname-utils:subdirectory to (pathname-utils:directory-name dir))))
    (ensure-directories-exist base)
    (dolist (file (uiop:directory* (merge-pathnames pathname-utils:*wild-file* dir)))
      (cond ((pathname-utils:directory-p file)
             (copy-dir file base))
            (T
             (uiop:copy-file file (merge-pathnames (pathname-utils:to-file file) base)))))))

(defun deploy-resources ()
  (let ((path qtools:*deployment-location*))
    (dolist (pool (pools))
      (qtools::status 1 "Copying pool ~a from ~a" (name pool) (base pool))
      (copy-dir (base pool) path))))
