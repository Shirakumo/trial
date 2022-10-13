#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () NIL)
  (deploy:copy-directory-tree (pathname-utils:subdirectory (root) "lang") directory)
  (when (probe-file (merge-pathnames "keymap.lisp" (root)))
    (uiop:copy-file (merge-pathnames "keymap.lisp" (root)) (merge-pathnames "keymap.lisp" directory)))
  ;; FIXME: This is bad. We always deploy a bunch of shit that's not really needed.
  (dolist (pool (list-pools))
    (let ((source (base pool)))
      ;; FIXME: We're potentially introducing conflicts here by eagerly coercing names.
      (setf (base pool) (make-pathname :directory (list :relative "pool" (string-downcase (name pool)))))
      (deploy:status 1 "Copying pool ~a from ~a" pool source)
      (deploy:copy-directory-tree
       source
       (merge-pathnames (base pool) directory)
       :copy-root NIL))))

(deploy:define-hook (:build trial) ()
  (v:remove-global-controller)
  ;; Finalize all subclasses of shader-entity to avoid shader recompilations
  (apply-class-changes (find-class 'shader-entity))
  ;; Fix version
  (let ((version (version :app)))
    (defmethod version ((_ (eql :app)))
      version)))

(deploy:define-hook (:boot trial) ()
  (v:restart-global-controller)
  (setf *random-state* (make-random-state T)))

(defmacro dont-deploy (&rest libraries)
  `(progn ,@(loop for lib in libraries
                  collect `(deploy:define-library ,lib :dont-deploy T))))

(dont-deploy
 cl-opengl-bindings::opengl)
#+linux
(dont-deploy
 org.shirakumo.fraf.gamepad.impl::evdev)
#+darwin
(dont-deploy
 org.shirakumo.fraf.gamepad.impl::corefoundation
 org.shirakumo.fraf.gamepad.impl::iokit
 org.shirakumo.fraf.gamepad.impl::forcefeedback
 org.shirakumo.messagebox::foundation
 org.shirakumo.messagebox::appkit
 org.shirakumo.messagebox::cocoa)
#+windows
(dont-deploy
 org.shirakumo.com-on.cffi::ole32
 org.shirakumo.fraf.gamepad.impl::user32
 org.shirakumo.fraf.gamepad.impl::xinput
 org.shirakumo.fraf.gamepad.impl::dinput
 org.shirakumo.messagebox::user32)
