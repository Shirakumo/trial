#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *fps* 1/30)
(defvar *main-window* NIL)

(define-widget main (QGLWidget)
  ((scene :initform (make-instance 'scene) :accessor scene :finalized T)))

(define-initializer (main setup)
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) "Trial"))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (with-simple-restart (abort "Abort the update and continue.")
      (issue scene 'tick)
      (process scene))
    (q+:update main)
    (q+:start timer 
              (round (max 0 (* (- *fps* (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second))
                               1000))))))

(define-override (main "initializeGL" initialize-gl) ()
  (q+:qgl-clear-color main background)
  (gl:enable :depth-test)
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:clear-depth 1)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :smooth)
  (gl:front-face :ccw)
  (gl:hint :perspective-correction-hint :nicest)
  (add-subject (make-instance 'cat) scene))

(define-override (main "resizeGL" resize-gl) (width height)
  (with-simple-restart (abort "Abort the resize and continue.")
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (set-perspective 45 (/ width (max 1 height)) 0.1 1000)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:viewport 0 0 width height)))

(define-override (main "paintGL" paint-gl) ()
  (with-simple-restart (abort "Abort the drawing and continue.")
    (gl:clear :color-buffer :depth-buffer)
    (gl:load-identity)
    ;; FIXME: Move into camera code
    (gl:translate 0 0 -10)
    (draw scene)
    (gl:flush)))

(defun main ()
  (with-main-window (window 'main #-darwin :main-thread #-darwin NIL)))

(defun set-perspective (fovy aspect z-near z-far)
  ;; http://nehe.gamedev.net/article/replacement_for_gluperspective/21002/
  (let* ((fh (* (tan (* (/ fovy 360) PI)) z-near))
         (fw (* fh aspect)))
    (gl:frustum (- fw) fw (- fh) fh z-near z-far)))
