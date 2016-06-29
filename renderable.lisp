#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass renderable ()
  ((thread :initform NIL :accessor thread)
   (last-pause :initform 0 :accessor last-pause)
   (fps :initarg :fps :accessor fps))
  (:default-initargs
   :name :controller
   :fps 30.0f0))

(defmethod initialize-instance :after ((renderable renderable) &key)
  (setf (thread renderable) T)
  (setf (thread renderable)
        (with-thread ("renderable thread")
          (render-loop renderable))))

(defmethod finalize :before ((renderable renderable))
  (let ((thread (thread renderable)))
    (with-thread-exit (thread)
      (setf (thread renderable) NIL))))

(defun pause-time (fps start)
  (let* ((duration (max 0 (- (current-time) start)))
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

(defmethod render (thing (renderable renderable)))

(defun render-loop (renderable)
  (unwind-protect
       (with-error-logging (:trial.renderable "Error in render thread")
         (loop while (thread renderable)
               do (with-simple-restart (abort "Abort the update and retry.")
                    (setf (last-pause renderable)
                          (with-frame-pause ((fps renderable))
                            (render renderable renderable))))))    
    (v:info :trial.renderable "Exiting render-loop for ~a." renderable)))
