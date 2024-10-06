(in-package #:org.shirakumo.fraf.trial)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug-features* '(:check-global-bounds-cache-obb))
  (defparameter *optimize-features* '(:elide-buffer-access-checks
                                      :elide-coercion-size-checks
                                      :elide-container-checks
                                      :elide-allocation-checks
                                      :elide-handler-restarts
                                      :elide-context-current-checks))

  #+trial-debug-all
  (setf *features* (union *features* *debug-features*))

  #+trial-debug-none
  (setf *features* (set-difference *features* *debug-features*))

  #+trial-optimize-all
  (setf *features* (union *features* *optimize-features*))

  #+trial-optimize-none
  (setf *features* (set-difference *features* *optimize-features*)))

(defun enable-debug-features (&rest features)
  (setf *features* (union *features* (or features *debug-features*))))

(defun reload-with-features (&rest features)
  (setf *features* (union *features* features))
  (asdf:compile-system :trial :force T :verbose NIL)
  (asdf:load-system :trial :force T :verbose NIL))

;; FIXME: Put all the consistency checks and such during loading etc under features.

(when (char= #\Return (char "
" 0))
  (error "!! GIT FUCKED YOU OVER !!

Fix GIT to not convert line endings to CRLF:

  git config --global core.autocrlf false
  git add --renormalize .

By the way, I'd like to punch whoever added that feature to
GIT and made it default in the fucking throat."))

#-3d-math-f32
(error "Trial does not work without single-float math.")

(pushnew :trial *features*)
