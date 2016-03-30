#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *main-window* NIL)

(define-widget main (QGLWidget)
  ((scene :initform (make-instance 'scene) :accessor scene :finalized T)))

(define-initializer (main setup)
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) "Trial"))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (with-simple-restart (abort "Abort the update and continue.")
      ;; FIXME: Move this to a more appropriate place where it isn't called so frequently.
      (cl-gamepad:detect-devices)
      (cl-gamepad:process-events)
      (issue scene 'tick)
      (process scene))
    (q+:update main)
    (let ((pause (round (pause-miliseconds start))))
      (if (= 0 pause)
          (q+:update main)
          (q+:start timer pause)))))

(define-override (main "initializeGL" initialize-gl) ()
  (q+:qgl-clear-color main background)
  (gl:enable :depth-test :blend :cull-face :texture-2d)
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:clear-depth 1.0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :smooth)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:hint :perspective-correction-hint :nicest)
  (add-subject (make-instance 'cat) scene))

(define-override (main "resizeGL" resize-gl) (width height)
  (with-simple-restart (abort "Abort the resize and continue.")
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (set-perspective 45 (/ width (max 1 height)) 0.01 1000.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:viewport 0 0 width height)))

(define-override (main "paintGL" paint-gl) ()
  (with-simple-restart (abort "Abort the drawing and continue.")
    (gl:clear :color-buffer :depth-buffer)
    (gl:load-identity)
    (gl:enable :depth-test :blend :cull-face :texture-2d)
    ;; FIXME: Move into camera code
    (gl:translate 0 -30 -200)
    (draw scene)
    (gl:flush)))

(defun main ()
  (with-main-window (window 'main #-darwin :main-thread #-darwin NIL)))

(defun set-perspective (fovy aspect z-near z-far)
  ;; http://nehe.gamedev.net/article/replacement_for_gluperspective/21002/
  (let* ((fh (* (tan (* (/ fovy 360) PI)) z-near))
         (fw (* fh aspect)))
    (gl:frustum (- fw) fw (- fh) fh z-near z-far)))
