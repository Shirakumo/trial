#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  ;; FIXME: This is bad. We always deploy a bunch of shit that's not really needed.
  (dolist (pool (list-pools))
    (let ((source (base pool)))
      ;; FIXME: We're potentially introducing conflicts here by eagerly coercing names.
      (setf (base pool) (make-pathname :directory (list :relative "pool" (string (name pool)))))
      (deploy:status 1 "Copying pool ~{~a ~}from ~a" pools source)
      (deploy:copy-directory-tree
       source
       (merge-pathnames (base pool) directory)
       :copy-root NIL))))

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
   org.shirakumo.com-on.cffi::ole32
   org.shirakumo.fraf.gamepad.impl::user32
   org.shirakumo.fraf.gamepad.impl::xinput
   org.shirakumo.fraf.gamepad.impl::dinput))
