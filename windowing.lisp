#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *fps* 1/30)
(defvar *main-window* NIL)

(define-widget main (QGLWidget)
  ((game-objects :initform NIL :accessor objects)))

(define-initializer (main setup)
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) "Trial"))

(define-finalizer (main teardown)
  (loop for obj in (objects main)
        do (finalize obj)))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (with-simple-restart (abort "Abort the update and continue.")
      (issue 'tick)
      (process *event-loop*)
      (loop for obj in (objects main) ;; remove this later as it will be handled through *event-loop*
            do (update obj)))
    (q+:repaint main)
    (q+:start timer 
              (round (max 0 (* (- *fps* (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second))
                               1000))))))

(define-override (main "initializeGL" initialize-gl) ()
  (push (make-instance 'game-object :path "test.jpg") (objects main)))

(define-override (main paint-event) (ev)
  (declare (ignore ev))
  (with-simple-restart (abort "Abort the drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (q+:fill-rect painter (q+:rect main) bgbrush)

      (q+:begin-native-painting painter)

      (loop for obj in (objects main)
            do (draw obj))
      
      (q+:end-native-painting painter))))

(defun main ()
  (deeds:start *event-loop*)
  (unwind-protect
       (with-main-window (window 'main #-darwin :main-thread #-darwin NIL))
    (deeds:stop *event-loop*)))
