#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  (dolist (pool (list-pools))
    (deploy:status 1 "Copying pool ~a from ~a" (name pool) (base pool))
    ;; FIXME: This is really, really bad. Pools that are based off the same thing
    ;;        will copy all the assets every time, duplicating things a lot.
    ;;        we also always deploy a bunch of shit that's not really needed.
    (deploy:copy-directory-tree
     (pool-path pool NIL)
     (pathname-utils:subdirectory directory "pool" (package-name (symbol-package (name pool))) (string-downcase (name pool)))
     :copy-root NIL))
  (setf *standalone* T))

(deploy:define-hook (:build trial) ()
  (v:remove-global-controller))

(deploy:define-hook (:boot trial) ()
  (v:restart-global-controller)
  (setf *random-state* (make-random-state T)))

(deploy:define-library cl-opengl-bindings::opengl
  :dont-deploy T)

#+(and trial-mmap (not windows))
(deploy:define-library osicat-posix::librt
  :dont-deploy T)
