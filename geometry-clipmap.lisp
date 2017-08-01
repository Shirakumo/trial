#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun make-clipmap-vaos (n)
  (let ((m (/ (1+ n) 4)))
    (change-class (make-quad-grid x m m) 'vertex-array)
    ))
