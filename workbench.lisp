(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames)
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench cat) image
    #p"cat.png")

(define-asset (workbench cube) mesh
    (make-cube 15))

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-shader-entity player (vertex-entity textured-entity transformed-entity listener)
  ((name :initform 'player)
   (texture :initform (// 'workbench 'cat))
   (vertex-array :initform (// 'workbench 'cube))))

(define-handler (player tick) (dt)
  (when (retained :w)
    (incf (vz (location player)) (* dt +50)))
  (when (retained :a)
    (incf (vx (location player)) (* dt -50)))
  (when (retained :s)
    (incf (vz (location player)) (* dt -50)))
  (when (retained :d)
    (incf (vx (location player)) (* dt +50))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'trial::fps-counter) scene)
    (enter (make-instance 'debug-text :text "HELLO and welcome back to VIDEO GAMES") scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'player :location (vec 0 100 200)) scene)
    (enter (make-instance 'following-camera :target (unit 'player scene) :location (vec 0 10 100)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))

