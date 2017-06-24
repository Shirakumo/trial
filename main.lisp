#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: Fullscreenable seems to cause really bad behaviour, idk
;; FIXME: Re-add hud somehow
(defclass main (display window)
  ((scene :initform (make-instance 'scene) :accessor scene)
   (pipeline :initform (make-instance 'pipeline :name :pipeline) :accessor pipeline)
   (controller :initform (make-instance 'controller) :accessor controller))
  (:default-initargs
   :name :main))

(defmethod initialize-instance :after ((main main) &key)
  (with-slots (scene pipeline controller) main
    (setf (display controller) main)
    (register pipeline scene)
    (register controller scene)
    (issue scene 'reload-scene)
    (start scene)))

(defmethod finalize ((main main))
  (with-slots (scene pipeline controller) main
    (v:info :trial.main "RAPTURE")
    (acquire-context (context main) :force T)
    (finalize controller)
    (finalize pipeline)
    (finalize scene)))

(defmethod handle (event (main main))
  (issue (scene main) event))

(defmethod update ((main main) tt dt)
  (issue (scene main) 'tick :tt tt :dt dt)
  (process (scene main)))

(defmethod setup-scene :around ((main main))
  (gl:clear :color-buffer)
  (swap-buffers (context main))
  (stop (scene main))
  (reset (scene main))
  (clear (pipeline main))
  (with-simple-restart (continue "Skip loading the rest of the scene and hope for the best.")
    (v:info :trial.main "Setting up scene")
    (with-timing-report (info :trial.main "Scene setup took ~fs run time, ~fs clock time.")
      (call-next-method)))
  (load (scene main))
  (load (pipeline main))
  (start (scene main))
  ;; Cause camera to refresh
  (issue (scene main) 'resize :width (width main) :height (height main)))

;; FIXME: proper LOADing of a map
(defmethod setup-scene ((main main))
  ())

(defmethod setup-scene :after ((main main))
  (enter (controller main) (scene main))
  (setup-pipeline main))

(defmethod setup-pipeline ((main main))
  ())

(defmethod setup-pipeline :after ((main main))
  (pack-pipeline (pipeline main) main)
  (loop for pass across (passes (pipeline main))
        do (for:for ((element over (scene main)))
             (register-object-for-pass pass element))))

(defmethod paint ((source main) (target main))
  (paint (pipeline source) target))

(defun launch (&optional (main 'main) &rest initargs)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  (standalone-logging-handler)
  (handler-bind ((error #'standalone-error-handler))
    (apply #'launch-with-context main initargs))
  (tg:gc :full T))
