(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity video-player (transformed-entity video)
  ((name :initform :video)
   (state :initform :playing)))

(define-handler (video-player text-entered) (text)
  (case (char text 0)
    ((#\p #\Space)
     (if (eql :playing (state video-player))
         (setf (state video-player) :paused)
         (setf (state video-player) :playing)))
    ((#\r)
     )))

(define-example video
  (gl:clear-color 0 0 0 0)
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'video-player :asset (assets:asset :hello) :location (vec 0 2 0) :scaling (vec 5 5 5)) scene)
  (observe! (clock (node :video T)) :title "Time")
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2 7) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'phong-render-pass) scene))
