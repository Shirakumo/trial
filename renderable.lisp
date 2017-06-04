#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass renderable ()
  ((thread :initform NIL :accessor thread)
   (last-pause :initform 0.0s0 :accessor last-pause)
   (last-duration :initform 0.0s0 :accessor last-duration)
   (target-fps :initarg :target-fps :accessor target-fps)
   (actual-fps :initform 1.0s0 :accessor actual-fps))
  (:default-initargs
   :target-fps 30.0s0))

(defmethod start ((renderable renderable))
  (setf (thread renderable) T)
  (setf (thread renderable)
        (with-thread ("renderable thread")
          (render-loop renderable))))

(defmethod stop ((renderable renderable))
  (let ((thread (thread renderable)))
    (with-thread-exit (thread)
      (setf (thread renderable) NIL))))

(defmethod finalize :before ((renderable renderable))
  (stop renderable))

(declaim (inline call-with-frame-pause))
(defun call-with-frame-pause (function renderable)
  (let* ((start (current-time))
         (fps (target-fps renderable)))
    (declare (type single-float fps))
    (funcall function)
    (let* ((duration (/ (max 0.0s0 (- (current-time) start)) *time-units*)))
      (setf (last-duration renderable) duration)
      (when (< 0.0s0 fps)
        (let ((remainder (max 0.0s0 (- (/ fps) duration))))
          (setf (last-pause renderable) remainder)
          (sleep remainder)))
      (setf (actual-fps renderable) (/ *time-units* (max 1.0s0 (- (current-time) start)))))))

(defmacro with-frame-pause ((renderable) &body body)
  `(call-with-frame-pause (lambda () ,@body) ,renderable))

(defmethod render (thing (renderable renderable)))

(defun render-loop (renderable)
  (unwind-protect
       (with-error-logging (:trial.renderable "Error in render thread")
         (loop while (thread renderable)
               do (with-simple-restart (abort "Abort the update and retry.")
                    (with-frame-pause (renderable)
                      (render renderable renderable)))))    
    (v:info :trial.renderable "Exiting render-loop for ~a." renderable)))
