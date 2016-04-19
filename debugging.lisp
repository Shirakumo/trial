#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *debug-features* '(:trial-debug-selection-buffer
                           :trial-debug-bound-subject))

#+trial-debug-all
(setf *features* (union *features* *debug-features*))

#+trial-debug-none
(setf *features* (set-difference *features* *debug-features*))

(defun reload-debugged (&rest features)
  (setf *features* (union *features* features))
  (asdf:compile-system :trial :force T :verbose NIL)
  (asdf:load-system :trial :force T :verbose NIL))
