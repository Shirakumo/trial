#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass renderable ()
  ((thread :initform NIL :accessor thread)
   (delta-time :initarg :delta-time :initform 0.01d0 :accessor delta-time)
   (frame-time :initform 0.0d0 :accessor frame-time)))

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
(defmethod update ((renderable renderable) tt dt fc))

(defmethod render-loop ((renderable renderable))
  (declare (optimize speed))
  (let ((fc 0))
    (declare (type fixnum fc))
    (with-simple-restart (exit-render-loop "Exit the render loop entirely.")
      (unwind-protect
           (with-retry-restart (reset-render-loop "Reset the render loop timing, not catching up with lost frames.")
             (let ((tt 0.0d0)
                   (dt (coerce (delta-time renderable) 'double-float))
                   (current-time (current-time))
                   (accumulator 0.0d0)
                   (new-time 0.0d0)
                   (frame-time 0.0d0))
               (declare (type double-float tt dt current-time
                              accumulator new-time frame-time))
               (with-error-logging (:trial.renderable "Error in render thread")
                 (loop while (thread renderable)
                       do (setf new-time (current-time))
                          (setf frame-time (- new-time current-time))
                          (setf current-time new-time)
                          (incf accumulator frame-time)
                          (loop while (<= dt accumulator)
                                do (update renderable tt dt fc)
                                   (decf accumulator dt)
                                   (incf tt dt))
                          ;; FIXME: interpolate state
                          ;;        See http://gafferongames.com/game-physics/fix-your-timestep/
                          (setf (frame-time renderable) frame-time)
                          (with-simple-restart (abort "Abort the update and retry.")
                            (render renderable renderable)
                            (incf fc))))))
        (v:info :trial.renderable "Exiting render-loop for ~a." renderable)))))
