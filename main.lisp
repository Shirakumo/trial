#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *main* NIL)

(define-subject main-controller (controller)
  ())

(define-handler (main-controller launch-editor) (ev)
  (signal! (display main-controller) (launch-editor)))


(define-widget main (QGLWidget display)
  ((controller :initform (make-instance 'main-controller :display NIL))
   (input-handler :initform (make-instance 'input-handler))))

(define-initializer (main setup)
  (setf *main* main)
  (setf (q+:window-title main) "Trial")
  (add-handler main input-handler))

(define-finalizer (main teardown)
  (v:info :trial.main "RAPTURE")
  (dolist (pool (pools))
    (mapc #'offload (assets pool)))
  (setf *main* NIL))

(define-signal (main launch-editor) ())

(define-slot (main launch-editor) ()
  (declare (connected main (launch-editor)))
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor) main)))

;; FIXME: proper LOADing of a map
(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    ;;(enter (make-instance 'skybox) scene)
    (enter (make-instance 'space-axes) scene)
    (enter (make-instance 'player) scene)
    (enter (make-instance 'following-camera :name :camera :target (unit :player scene)) scene)
    (enter (make-instance 'selection-buffer :name :selection-buffer) scene)))

(defun launch (&rest initargs)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (unwind-protect
       (with-main-window (window (apply #'make-instance 'main initargs)
                          #-darwin :main-thread #-darwin NIL))))

(defun launch-with-launcher (&rest initargs)
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (ensure-qapplication)
  (cl-monitors:init)
  (unwind-protect
       (let ((opts NIL))
         (with-finalizing ((launcher (make-instance 'launcher)))
           (with-main-window (w launcher #-darwin :main-thread #-darwin NIL))
           (setf opts (init-options launcher)))
         (apply #'launch (append initargs opts)))
    (cl-monitors:deinit)))
