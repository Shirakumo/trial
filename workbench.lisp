(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:v #:org.shirakumo.verbose))
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

(defclass workbench (main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)
                     :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(defmethod update ((main workbench) tt dt fc)
  (if (paused-p main)
      (handle (make-event 'tick :tt tt :dt dt :fc fc) (camera (scene main)))
      (issue (scene main) 'tick :tt tt :dt dt :fc fc))
  (process (scene main)))

(define-pool workbench)

(define-asset (workbench cat) image
    #p"cat.png")

(define-asset (workbench cube) mesh
    (make-cube-mesh 10))

(define-asset (workbench grid) mesh
    (make-line-grid-mesh 10 100 100))

(define-shader-entity cube (trial::single-material-renderable rigidbody listener)
  ((name :initform 'cube)
   (trial::material :initform (make-instance 'trial::phong-material))
   (vertex-array :initform (// 'workbench 'cube)))
  (:default-initargs
   :mass 10.0
   :physics-primitives (make-box :bsize (vec 5 5 5) :material :wood)))

(define-handler (cube tick) (dt)
  (let ((strength (* dt 10000)))
    (when (retained :a)
      (nv+ (torque cube) (vec strength 0 0)))
    (when (retained :d)
      (nv- (torque cube) (vec strength 0 0)))
    (when (retained :w)
      (nv+ (torque cube) (vec 0 strength 0)))
    (when (retained :s)
      (nv- (torque cube) (vec 0 strength 0)))))

(define-handler (controller text-entered) (text)
  (when (string= text "r")
    (trial:debug-clear)
    (for:for ((cube over (scene +main+))
              (i from 0))
      (when (typep cube 'cube)
        (vsetf (location cube) 0 (+ 5 (* i 11)) 0)
        (qsetf (orientation cube) 0 0 0 1)
        (vsetf (velocity cube) 0 0 0)
        (vsetf (rotation cube) 0 0 0)
        (setf (awake-p cube) T))))
  (when (string= text "p")
    (setf (paused-p +main+) (not (paused-p +main+))))
  (when (string= text "s")
    (issue (scene +main+) 'tick :tt 1.0d0 :dt 0.01 :fc 1)))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'display-controller) scene)
    (observe! "
[P] Pause
[R] Reset
[S] Step
[Ctrl] Look
[WASD] Move
[Space] Ascend
[C] Descend" :title "Controls")
    (let ((physics (make-instance 'rigidbody-system :units-per-metre 1.0))
          (floor (make-instance 'rigidbody :physics-primitives (trial::make-half-space :material :wood))))
      (enter floor physics)
      (loop for i from 0 below 10
            for cube = (make-instance 'cube :location (vec 0 (+ 5 (* i 11)) 0))
            do (enter cube physics)
               (enter cube scene))
      (enter (make-instance 'gravity :gravity (vec 0 -200 0)) physics)
      (enter physics scene))
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'vertex-entity :vertex-array (// 'workbench 'grid)) scene)
    (enter (make-instance 'editor-camera :rotation (vec 0 (* 1.5 PI) 0) :location (vec 100 10 0)) scene)
    (enter (make-instance 'trial::ambient-light) scene)
    (enter (make-instance 'trial::phong-render-pass) scene))
  (maybe-reload-scene))

