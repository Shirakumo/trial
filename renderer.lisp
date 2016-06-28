#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject renderer ()
  ((thread :initform NIL :accessor thread)
   (last-pause :initform 0 :accessor last-pause)
   (fps :initarg :fps :accessor fps)
   (target :initarg :target :accessor target))
  (:default-initargs
   :name :controller
   :fps 30.0f0
   :target (error "TARGET required.")))

(defmethod initialize-instance :after ((renderer renderer) &key target)
  (etypecase target (context))
  (setf (thread renderer)
        (with-thread ("renderer thread")
          (render target renderer))))

(defmethod finalize :after ((renderer renderer))
  (let ((thread (thread renderer)))
    (with-thread-exit (thread)
      (setf (thread renderer) NIL))))

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

(defmethod render (target (renderer renderer))
  (unwind-protect
       (with-error-logging (:trial.renderer "Error in render thread")
         (loop while (thread renderer)
               do (with-simple-restart (abort "Abort the update and retry.")
                    (setf (last-pause renderer)
                          (with-frame-pause ((fps renderer))
                            (render target target))))))    
    (v:info :trial.renderer "Exiting render-loop for ~a." target)))
