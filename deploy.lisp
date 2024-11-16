(in-package #:org.shirakumo.fraf.trial)

(deploy:define-hook (:deploy trial) (directory)
  (deploy:copy-directory-tree (pathname-utils:subdirectory (data-root) "lang") directory)
  (let ((default-keymap (pathname-utils:merge-pathnames* "keymap.lisp" (data-root))))
    (when (probe-file default-keymap)
      (uiop:copy-file default-keymap (pathname-utils:merge-pathnames* "keymap.lisp" directory))))
  (dolist (pool (list-pools))
    (let* ((source (base pool))
           (unused (loop for path in (unused-file-patterns pool)
                         collect (pathname-utils:merge-pathnames* path source))))
      (flet ((unused-file-p (src dst)
               (declare (ignore dst))
               (loop for pattern in unused
                     thereis (if (pathname-utils:directory-p pattern)
                                 (pathname-utils:subpath-p src pattern)
                                 (pathname-utils:pathname-matches-p src pattern)))))
        ;; FIXME: We're potentially introducing conflicts here by eagerly coercing names.
        (setf (base pool) (make-pathname :directory (list :relative "pool" (string-downcase (name pool)))))
        (deploy:status 1 "Copying pool ~a from ~a" pool source)
        (deploy:copy-directory-tree
         source
         (pathname-utils:merge-pathnames* (base pool) directory)
         :copy-root NIL
         :exclude #'unused-file-p)))))

(deploy:define-hook (:build trial) ()
  (v:remove-global-controller)
  ;; Finalize all subclasses of shader-entity to avoid shader recompilations
  (apply-class-changes (find-class 'shader-entity))
  (labels ((recurse (class)
             (c2mop:finalize-inheritance class)
             (dolist (sub (c2mop:class-direct-subclasses class))
               (recurse sub))))
    (recurse (find-class 'shader-entity)))
  ;; Fix versions to eliminate source dependency
  (let ((version (version :app))) (defmethod version ((_ (eql :app))) version))
  (let ((version (version :trial))) (defmethod version ((_ (eql :trial))) version)))

#+asdf
(deploy:define-hook (:build neuter-asdf #.MOST-NEGATIVE-FIXNUM) ()
  (asdf:clear-configuration)
  (setf (fdefinition 'asdf:upgrade-asdf) (lambda ()))
  #+quicklisp (setf ql:*local-project-directories* ())
  (dolist (system (asdf:already-loaded-systems))
    (asdf:register-immutable-system system)
    (asdf:clear-system system)))

(deploy:define-hook (:boot trial) ()
  (v:restart-global-controller)
  (setf *random-state* (make-random-state T))
  (random:reseed random:*generator* T))

(defmacro dont-deploy (&rest libraries)
  `(progn ,@(loop for lib in libraries
                  collect `(deploy:define-library ,lib :dont-deploy T))))

(dont-deploy
 cl-opengl-bindings::opengl)
#+linux
(deploy:define-library org.shirakumo.fraf.gamepad.impl::evdev
  :path (asdf:system-relative-pathname :cl-gamepad "static/libevdev-lin-amd64.so"))
#+darwin
(dont-deploy
 org.shirakumo.fraf.gamepad.impl::corefoundation
 org.shirakumo.fraf.gamepad.impl::iokit
 org.shirakumo.fraf.gamepad.impl::forcefeedback
 org.shirakumo.messagebox.macos::foundation
 org.shirakumo.messagebox.macos::appkit
 org.shirakumo.messagebox.macos::cocoa)
#+windows
(dont-deploy
 secur32
 org.shirakumo.com-on.cffi::ole32
 org.shirakumo.fraf.gamepad.impl::user32
 org.shirakumo.fraf.gamepad.impl::xinput
 org.shirakumo.fraf.gamepad.impl::dinput
 org.shirakumo.messagebox.win32::user32
 org.shirakumo.machine-state::psapi
 org.shirakumo.machine-state::ntdll)
