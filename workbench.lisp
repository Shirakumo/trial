(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames)
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass movement (directional-action)
  ())

(defmethod active-p ((action movement)) T)

(load-mapping '((directional movement
                 (point)
                 (stick :one-of ((:l-h :l-v) (:r-h :r-v) (:dpad-h :dpad-v)))
                 (keys :one-of ((:w :a :s :d) (:i :j :k :l)))
                 (buttons :one-of ((:y :x :a :b))))))

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
  (incf (vx (location player)) (* dt 50 (vx (directional 'movement))))
  (incf (vz (location player)) (* dt 50 (vy (directional 'movement)))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'trial::fps-counter) scene)
    (enter (make-instance 'debug-text :text "HELLO and welcome back to VIDEO GAMES") scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'player :location (vec 0 100 200)) scene)
    (enter (make-instance 'following-camera :target (unit 'player scene) :location (vec 0 10 100)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))

