#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject main-controller (controller)
  ())

(define-handler (main-controller launch-editor) (ev)
  (signal! (display main-controller) (launch-editor)))


(define-widget main (QGLWidget display input-handler fullscreenable executable window)
  ((scene :initform (make-instance 'scene) :accessor scene)
   (controller :initform (make-instance 'main-controller))
   (input-handler :initform (make-instance 'input-handler)))
  (:default-initargs
   :name :main))

(define-initializer (main setup)
  (setf (q+:window-title main) "Trial")
  (add-handler main input-handler)
  (enter controller scene)
  (issue scene 'reload-scene)
  (start scene))

(define-finalizer (main teardown)
  (v:info :trial.main "RAPTURE")
  (acquire-context display :force T)
  (finalize controller)
  (finalize scene)
  (dolist (pool (pools))
    (mapc #'offload (assets pool))))

(define-signal (main launch-editor) ())

(define-slot (main launch-editor) ()
  (declare (connected main (launch-editor)))
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor) main)))

(defmethod handle (event (main main))
  (issue event (scene main)))

(defmethod setup-scene :around ((display display))
  (with-simple-restart (continue "Skip loading the rest of the scene and hope for the best.")
    (call-next-method)))

;; FIXME: proper LOADing of a map
(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    ;;(enter (make-instance 'skybox) scene)
    (enter (make-instance 'space-axes) scene)
    (enter (make-instance 'player) scene)
    (enter (make-instance 'following-camera :name :camera :target (unit :player scene)) scene)
    (enter (make-instance 'selection-buffer :name :selection-buffer) scene)))

(defmethod paint ((source main) (target display))
  (paint (scene source) target))

(defun launch (&rest initargs)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (with-main-window (window (apply #'make-instance 'main initargs)
                     #-darwin :main-thread #-darwin NIL)))

(defun launch-with-launcher (&rest initargs)
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (ensure-qapplication)
  (let ((opts NIL))
    (with-finalizing ((launcher (make-instance 'launcher)))
      (with-main-window (w launcher #-darwin :main-thread #-darwin NIL))
      (setf opts (init-options launcher)))
    (apply #'launch (append initargs opts))))
