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

(macrolet ((dont-deploy (&rest libraries)
             `(progn ,@(loop for lib in libraries
                             collect `(deploy:define-library ,lib :dont-deploy T)))))
  (dont-deploy
   cl-opengl-bindings::opengl)
  #+linux
  (dont-deploy
   org.shirakumo.fraf.gamepad.impl::evdev)
  #+darwin
  (dont-deploy
   org.shirakumo.fraf.gamepad.impl::corefoundation
   org.shirakumo.fraf.gamepad.impl::iokit
   org.shirakumo.fraf.gamepad.impl::forcefeedback)
  #+windows
  (dont-deploy
   org.shirakumo.fraf.gamepad.impl::ole32
   org.shirakumo.fraf.gamepad.impl::user32
   org.shirakumo.fraf.gamepad.impl::xinput
   org.shirakumo.fraf.gamepad.impl::dinput))
