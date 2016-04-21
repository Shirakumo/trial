#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject clocked-subject (clock)
  ())

(define-handler (clocked-subject advance-time tick) (ev)
  (update clocked-subject))

(defmethod save-form-args append ((subject clocked-subject))
  `(:clock ,(clock subject)
    :running ,(running subject)))
