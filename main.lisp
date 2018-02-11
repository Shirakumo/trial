#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: Fullscreenable seems to cause really bad behaviour, idk
(defclass main (display)
  ((scene :initform (make-instance 'pipelined-scene) :accessor scene)
   (controller :initform (make-instance 'controller) :accessor controller)))

(defmethod initialize-instance :after ((main main) &key)
  (with-slots (scene controller) main
    (setf (display controller) main)
    (start scene)))

(defmethod finalize ((main main))
  (with-slots (scene controller) main
    (v:info :trial.main "RAPTURE")
    (acquire-context (context main) :force T)
    (finalize controller)
    (finalize scene)))

(defmethod handle (event (main main))
  (issue (scene main) event))

(defmethod update ((main main) tt dt)
  (issue (scene main) 'tick :tt tt :dt dt)
  (process (scene main)))

(defmethod setup-rendering :after ((main main))
  (setup-scene main (scene main))
  (transition NIL (scene main)))

(defmethod setup-scene :around ((main main) (scene scene))
  (v:info :trial.main "Setting up ~a" scene)
  (with-timing-report (info :trial.main "Scene setup took ~fs run time, ~fs clock time.")
    (call-next-method))
  (start scene)
  ;; Cause camera to refresh
  (issue scene 'resize :width (width main) :height (height main)))

(defmethod setup-scene ((main main) scene)
  ())

(defmethod setup-scene :after ((main main) (scene scene))
  (enter (controller main) scene))

(defmethod paint ((source main) (target main))
  (paint (scene source) target)
  (gl:bind-framebuffer :draw-framebuffer 0)
  (%gl:blit-framebuffer 0 0 (width source) (height source) 0 0 (width source) (height source)
                        (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                        (cffi:foreign-enum-value '%gl:enum :nearest)))

(defun launch (&optional (main 'main) &rest initargs)
  (standalone-logging-handler)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  (handler-bind ((error #'standalone-error-handler))
    (apply #'launch-with-context main initargs))
  (tg:gc :full T))
