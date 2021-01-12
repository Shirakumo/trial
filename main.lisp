#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: Fullscreenable seems to cause really bad behaviour, idk
(defclass main (display window gamepad-input-handler)
  ((scene :initform (make-instance 'pipelined-scene) :accessor scene)
   (controller :initform (make-instance 'controller) :accessor controller)
   (loader :initform (make-instance 'loader) :accessor loader)))

(defmethod initialize-instance :after ((main main) &key)
  (with-slots (scene controller) main
    (setf (display controller) main)))

(defmethod finalize ((main main))
  (with-slots (scene controller loader) main
    (v:info :trial.main "RAPTURE")
    (acquire-context (context main) :force T)
    (clear-retained)
    (finalize controller)
    (finalize scene)
    (finalize loader)))

(defmethod handle (event (main main))
  (issue (scene main) event))

(defmethod update ((main main) tt dt fc)
  (issue (scene main) 'tick :tt tt :dt dt :fc fc)
  (process (scene main)))

(defmethod commit (thing (main main) &rest args)
  (apply #'commit thing (loader main) args))

(defmethod setup-rendering :after ((main main))
  (restart-case
      (change-scene main (scene main) :old NIL)
    (abort ()
      :report "Don't set up the scene, leaving it empty."
      (clear (scene main)))))

(defmethod setup-scene :around ((main main) (scene scene))
  (v:info :trial.main "Setting up ~a" scene)
  (with-timing-report (info :trial.main "Scene setup took ~fs run time, ~fs clock time.")
    (call-next-method))
  ;; Cause camera to refresh
  (issue scene 'resize :width (width main) :height (height main))
  scene)

(defmethod setup-scene ((main main) scene)
  ())

(defmethod setup-scene :after ((main main) (scene scene))
  (enter (controller main) scene))

(defmethod change-scene ((main main) (new scene) &key (old (scene main)))
  (unless (eq old new)
    (when old (stop old))
    (setup-scene main new)
    (with-timing-report (info :trial.main "Commit took ~fs run time, ~fs clock time.")
      (when (commit new (loader main))
        (setf (scene main) new)))
    (start (scene main)))
  (values new old))

(defun enter-and-load (object container main)
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (enter* object container)
    (loop for pass across (passes (scene main))
          do (stage pass area))
    (unless (commit area (loader main) :unload NIL)
      (remove-from-pass object (scene main))
      (leave object container))))

(defmethod render ((source main) (target main))
  (render (scene source) NIL)
  ;; KLUDGE: This assumes a pipelined scene
  (blit-to-screen (scene source)))

(defun launch (main &rest initargs)
  (standalone-logging-handler)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  (handler-bind ((error #'standalone-error-handler))
    (apply #'launch-with-context main initargs))
  (setf *context* NIL)
  (tg:gc :full T))
