#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;; FIXME: Fullscreenable seems to cause really bad behaviour, idk
;; FIXME: Re-add hud somehow
(define-widget main (QGLWidget display input-handler executable window)
  ((scene :initform (make-instance 'scene) :accessor scene)
   (pipeline :initform (make-instance 'pipeline :name :pipeline) :accessor pipeline)
   (controller :initform (make-instance 'controller))
   (title :initform "Trial" :initarg :title :accessor title))
  (:default-initargs
   :name :main))

(define-initializer (main setup -10)
  (setf (q+:window-title main) (title main))
  (setf (display controller) main)
  (register pipeline scene)
  (register controller scene)
  (issue scene 'reload-scene)
  (start scene))

(defmethod (setf title) :after (title (main main))
  (setf (q+:window-title main) title))

(define-finalizer (main teardown)
  (v:info :trial.main "RAPTURE")
  (acquire-context main :force T)
  (finalize controller)
  (finalize pipeline)
  (finalize scene))

(define-override (main focus-in-event) (ev)
  ;; FIXME: too primitive, must account for menus and such at a later point.
  (issue scene 'resume)
  (stop-overriding))

(define-override (main focus-out-event) (ev)
  (issue scene 'pause)
  (stop-overriding))

(defmethod handle (event (main main))
  (issue (scene main) event))

(defmethod setup-scene :around ((main main))
  (with-simple-restart (continue "Skip loading the rest of the scene and hope for the best.")
    (v:info :trial.main "Setting up scene")
    (with-timing-report (info :trial.main "Scene setup took ~fs run time, ~fs clock time.")
      (call-next-method))))

;; FIXME: proper LOADing of a map
(defmethod setup-scene ((main main))
  ())

(defmethod setup-scene :after ((main main))
  (let ((scene (scene main))
        (pipeline (pipeline main)))
    (setup-pipeline main)
    ;; FIXME: suboptimal
    (dolist (pass (passes pipeline))
      (for:for ((element over scene))
        (register-object-for-pass pass element)))
    (load scene)
    (load pipeline)))

(defmethod setup-pipeline ((main main))
  ())

(defmethod setup-pipeline :after ((main main))
  (pack-pipeline (pipeline main) main))

(defmethod paint ((source main) (target main))
  (issue (scene target) 'tick)
  (process (scene target))
  (paint (pipeline source) target))

(defun launch (&optional (main 'main) &key initargs application-name)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (with-main-window (window (apply #'make-instance main initargs) :name application-name))
  (tg:gc :full T))

(defun launch-with-launcher (&optional (main 'main) &key initargs application-name)
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (ensure-qapplication)
  (let ((opts NIL))
    (with-finalizing ((launcher (make-instance 'launcher)))
      (with-main-window (w launcher #-darwin :main-thread #-darwin NIL))
      (setf opts (init-options launcher)))
    (launch main :initargs (append initargs opts) :application-name application-name)))
