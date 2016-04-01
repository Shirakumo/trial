#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller ()
  ;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
  ((tickcount :initform 0.0d0 :accessor tickcount)))

(define-handler (controller devices tick 100) (ev)
  (incf (tickcount controller))
  (when (mod (tickcount controller) *fps*)
    (cl-gamepad:detect-devices))
  (cl-gamepad:process-events))

(define-handler (controller devices T 100) (ev)
  (map-event event *loop*))
