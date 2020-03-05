#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  (let ((bases (make-hash-table :test 'equalp)))
    ;; FIXME: This is bad. We always deploy a bunch of shit that's not really needed.
    (dolist (pool (list-pools))
      (push pool (gethash (base pool) bases)))
    (loop for base being the hash-keys of bases
          for pools being the hash-values of bases
          do ;; FIXME: We're potentially introducing conflicts here by eagerly coercing names.
             (let ((base-name (if (pathnamep base)
                                  (intern (string (name (first pools))) "KEYWORD")
                                  base)))
               (dolist (pool pools)
                 (setf (base pool) base-name))
               (deploy:status 1 "Copying pool ~{~a ~}from ~a" pools (coerce-base base))
               (deploy:copy-directory-tree
                (coerce-base base)
                (pathname-utils:subdirectory directory "pool" (string base-name))
                :copy-root NIL)))))

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
