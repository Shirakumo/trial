#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller ()
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tickcount :initform 0.0d0 :accessor tickcount)
   (update-thread :initform NIL :accessor update-thread)
   (last-pause :initform 0 :accessor last-pause)
   (fps :initform 30.0f0 :accessor fps)))

(defmethod initialize-instance :after ((controller controller) &key)
  (setf (update-thread controller)
        (bt:make-thread (lambda () (update-loop controller))
                        :initial-bindings `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)))))

(defmethod finalize ((controller controller))
  (let ((thread (update-thread controller)))
    (setf (update-thread controller) NIL)
    (loop for i from 0
          while (bt:thread-alive-p thread)
          do (sleep 0.1)
             (when (< 10 i)
               (v:warn :trial.controller "Update loop did not exit gracefully.")
               (bt:destroy-thread thread)
               (return)))))

(defun pause-time (fps start)
  (let* ((duration (- (current-time) start))
         (remainder (- (/ fps) (/ duration *time-units*))))
    (max 0 remainder)))

(defmacro with-frame-pause ((fps) &body body)
  (let ((pause (gensym "PAUSE"))
        (start (gensym "START")))
    `(let ((,start (current-time)))
       ,@body
       (let ((,pause (pause-time ,fps ,start)))
         (sleep ,pause)
         ,pause))))

(defun update-loop (controller)
  (let ((main *main-window*))
    (with-slots-bound (controller controller)
      (q+:make-current main)
      (loop while (update-thread controller)
            do (with-simple-restart (abort "Abort the update and retry.")
                 (setf (last-pause controller)
                       (with-frame-pause ((fps controller))
                         (dolist (scene (loops controller))
                           (issue scene 'tick)
                           (process scene))
                         (draw main)
                         (draw-hud main)
                         (q+:swap-buffers main)))))))
  (v:debug :trial.controller "Exiting update-loop."))

(define-handler (controller resize resize) (ev width height)
  (resize *main-window* width height))

(define-handler (controller tick tick 100) (ev)
  (incf (tickcount controller))
  (when (mod (tickcount controller) (fps controller))
    (cl-gamepad:detect-devices))
  (cl-gamepad:process-events))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*))

(defmethod draw-hud ((controller controller))
  (q+:render-text *main-window* 20 20 (format NIL "Pause: ~a" (last-pause controller))))
