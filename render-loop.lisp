#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass render-loop ()
  ((thread :initform NIL :accessor thread)
   (delta-time :initarg :delta-time :initform 0.01f0 :accessor delta-time)
   (frame-time :initform 0.0d0 :accessor frame-time)
   (target-frame-time :initarg :target-frame-time :initform 0.0d0 :accessor target-frame-time)))

(defmethod start ((render-loop render-loop))
  (setf (thread render-loop) T)
  (setf (thread render-loop)
        (with-thread ("render-loop")
          (render-loop render-loop))))

(defmethod stop ((render-loop render-loop))
  (let ((thread (thread render-loop)))
    (with-thread-exit (thread)
      (setf (thread render-loop) NIL))))

(defmethod finalize :before ((render-loop render-loop))
  (stop render-loop))

(defmethod render (thing (render-loop render-loop)))
(defmethod update ((render-loop render-loop) tt dt fc))

(defmethod render-loop ((render-loop render-loop))
  (declare (optimize speed))
  (let ((fc 0))
    (declare (type fixnum fc))
    (restart-case
        (unwind-protect
             (with-retry-restart (reset-render-loop "Reset the render loop timing, not catching up with lost frames.")
               (let ((tt 0.0d0)
                     (dt (coerce (delta-time render-loop) 'double-float))
                     (target-frame-time (coerce (target-frame-time render-loop) 'double-float))
                     (current-time (current-time))
                     (accumulator 0.0d0)
                     (new-time 0.0d0)
                     (frame-time 0.0d0)
                     (extra-time 0.0d0))
                 (declare (type double-float tt dt current-time
                                accumulator new-time frame-time))
                 (with-error-logging (:trial.render-loop "Error in render thread")
                   (loop while (thread render-loop)
                         do (loop (setf new-time (current-time))
                                  (setf frame-time (- new-time current-time))
                                  (setf extra-time (- target-frame-time frame-time))
                                  (when (<= extra-time 0.0)
                                    (return))
                                  (sleep extra-time))
                            (setf current-time new-time)
                            (incf accumulator frame-time)
                            (loop while (<= dt accumulator)
                                  do (when (<= 10d0 accumulator)
                                       (setf accumulator dt))
                                     (update render-loop tt dt fc)
                                     (decf accumulator dt)
                                     (incf tt dt)
                                     (incf fc))
                            ;; FIXME: interpolate state
                            ;;        See http://gafferongames.com/game-physics/fix-your-timestep/
                            (setf (frame-time render-loop) frame-time)
                            (with-simple-restart (abort "Abort the update and retry.")
                              (render render-loop render-loop))))))
          (v:info :trial.render-loop "Exiting render-loop for ~a." render-loop))
      (exit-render-loop ()
        :report "Exit the render loop entirely."
        (quit *context*)))))
