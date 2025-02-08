(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity simple-video-player (transformed-entity video-player)
  ((name :initform :video)
   (state :initform :playing)))

(define-handler ((player simple-video-player) text-entered) (text)
  (case (char text 0)
    ((#\p #\Space)
     (if (eql :playing (state player))
         (setf (state player) :paused)
         (setf (state player) :playing)))
    ((#\r)
     (seek player 0.0))
    ((#\l)
     (setf (loop-p player) (not (loop-p player))))))

(define-example video
  :title "Video Playback"
  :description "Showcases video playback of a simple animation."
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'simple-video-player :asset (assets:asset :hello) :location (vec 0 2 0) :scaling (vec 5 5 5)) scene)
  (observe! (clock (node :video T)) :title "Time [r]")
  (observe! (state (node :video T)) :title "State [p]")
  (observe! (loop-p (node :video T)) :title "Loop [l]")
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2 7) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'render-pass) scene))
