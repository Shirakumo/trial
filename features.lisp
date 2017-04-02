#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug-features* '(:trial-debug-selection-buffer
                             :trial-debug-controller))
  (defvar *optimize-features* '())

  #+trial-debug-all
  (setf *features* (union *features* *debug-features*))

  #+trial-debug-none
  (setf *features* (set-difference *features* *debug-features*))

  #+trial-optimize-all
  (setf *features* (union *features* *optimize-features*))

  #+trial-optimize-none
  (setf *features* (set-difference *features* *optimize-features*)))

(defun reload-with-features (&rest features)
  (setf *features* (union *features* features))
  (asdf:compile-system :trial :force T :verbose NIL)
  (asdf:load-system :trial :force T :verbose NIL))
