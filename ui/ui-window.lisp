#|
This file is a part of trial
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-entity ui-window (highlightable-widget pane group)
  ())

(defmethod paint :after ((window ui-window) target)
  (loop for e across (objects window)
        do (paint e target)))
