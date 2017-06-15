#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass renderable ()
  ((thread :initform NIL :accessor thread)
   (delta-time :initarg :delta-time :accessor delta-time))
  (:default-initargs
   :delta-time 0.01))

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

(defmethod render (thing (renderable renderable)))

(defun render-loop (renderable)
  (let ((tt 0.0d0)
        (dt (delta-time renderable))
        (current-time (current-time))
        (accumulator 0.0d0)
        (new-time 0.0d0)
        (frame-time 0.0d0)
        (alpha 0.0d0))
    (declare (type double-float tt dt current-time
                   accumulator new-time frame-time alpha))
    (declare (optimize speed))
    (unwind-protect
         (with-error-logging (:trial.renderable "Error in render thread")
           (loop while (thread renderable)
                 do (setf new-time (current-time))
                    (setf frame-time (- current-time new-time))
                    (when (< 0.25d0 frame-time)
                      (setf frame-time 0.25d0))
                    (setf current-time new-time)
                    (setf accumulator (+ accumulator frame-time))
                    (loop while (<= dt accumulator)
                          do (issue (scene target) 'tick :time tt :delta dt)
                             (process (scene target))
                             (setf tt (+ tt dt))
                             (setf accumulator (- accumulator dt)))
                    (setf alpha (/ accumulator dt))
                    ;; FIXME: interpolate state
                    (with-simple-restart (abort "Abort the update and retry.")
                      (render renderable renderable))))    
      (v:info :trial.renderable "Exiting render-loop for ~a." renderable))))
