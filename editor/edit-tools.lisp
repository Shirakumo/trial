#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor-tools
  (:nicknames #:org.shirakumo.fraf.trial.editor.tools)
  (:use #:cl+qt)
  (:export #:tools))
(in-package #:org.shirakumo.fraf.trial.editor.tools)
(in-readtable :qtools)

(define-subject toolkit (trial::global-selection-buffer)
  ((mode :initform :move :accessor mode)))

(defmethod (setf selected) :after ((entity located-entity) (toolkit toolkit))
  (when (eql (mode toolkit) :move)
    ))

;; FIXME: Enter editing tool things that you can drag to move / scale / rotate and so forth.
;;        Just think 3Ds Max, really. That's what I want. And with some effort, that's what
;;        we're going to get too.
