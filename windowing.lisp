(in-package #:trial)
(in-readtable :qtools)

(defparameter *fps* 1/30)
(defvar *main-window* NIL)

(define-widget main (QGLWidget)
  ((angle :initform 0)
   (angle-delta :initform 1)
   (event-loop :initform (make-instance 'synchronous-event-loop) :reader event-loop)))

(define-initializer (main setup)
  (setf *main-window* main)
  (setf *event-loop* event-loop)
  (start event-loop)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) "Trial"))

(define-finalizer (main teardown)
  (stop event-loop))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (with-simple-restart (abort "Abort the update and continue.")
      (incf (slot-value main 'angle) (slot-value main 'angle-delta))
      (process event-loop))
    (q+:repaint main)
    (q+:start timer 
              (round (max 0 (* (- *fps* (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second))
                               1000))))))

(define-override (main paint-event) (ev)
  (declare (ignore ev))
  (with-simple-restart (abort "Abort the drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      #| Paint calls here |#

      (q+:begin-native-painting painter)
      
      (gl:push-matrix)
      (gl:translate 250 250 0)
      (gl:rotate angle 0 0 1)
      (gl:with-primitives :quads
        (gl:color 1 0 0)
        (gl:vertex -50 -50)
        (gl:color 0 1 0)
        (gl:vertex 50 -50)
        (gl:color 0 0 1)
        (gl:vertex 50 50)
        (gl:color 1 1 1)
        (gl:vertex -50 50))
      (gl:pop-matrix)
      
      (q+:end-native-painting painter))))

(defun main ()
  (with-main-window (window 'main #-darwin :main-thread #-darwin NIL)))
