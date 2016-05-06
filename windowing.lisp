#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QGLWidget display)
  ())

(define-initializer (main setup)
  (v:info :trial "GENESIS")
  (setf *main* main)
  (setf (q+:window-title main) "Trial"))

(define-finalizer (main teardown)
  (v:info :trial "RAPTURE")
  (dolist (pool (pools))
    (mapc #'offload (assets pool)))
  (setf *main* NIL))

;; FIXME! How to catch the launching event?
;; (define-handler (main launch-editor) (ev)
;;   (signal! main (launch-editor)))

(define-signal (main launch-editor) ())

(define-slot (main launch-editor) ()
  (declare (connected main (launch-editor)))
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor) main)))

(defun launch (&rest initargs)
  (v:output-here)
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (with-main-window (window (apply #'make-instance 'main initargs)
                            #-darwin :main-thread #-darwin NIL)))

;; FIXME: proper LOADing of a map
(defmethod setup-scene ((main main))
  ;;(enter (make-instance 'skybox) scene)
  (enter (make-instance 'space-axes) scene)
  (enter (make-instance 'player) scene)
  (enter (make-instance 'fps-camera :name :camera) scene)
  (enter (make-instance 'selection-buffer :name :selection-buffer) scene))
