#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  (dolist (pool (list-pools))
    (deploy:status 1 "Copying pool ~a from ~a" (name pool) (base pool))
    (deploy:copy-directory-tree
     (pool-path pool NIL)
     (pathname-utils:subdirectory directory "pool" (string-downcase (base pool)))
     :copy-root NIL))
  (setf *standalone* T))

(deploy:define-hook (:build trial) ()
  (cl-monitors:deinit)
  (shutdown-gamepad-system))

(deploy:define-hook (:boot trial) ()
  (cl-monitors:init)
  (init-gamepad-system))
