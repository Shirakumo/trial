(defpackage #:workbench
  (:nicknames #:trial-workbench #:org.shirakumo.fraf.trial.workbench)
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:v #:org.shirakumo.verbose))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass workbench (main)
  ((paused-p :initform NIL :accessor paused-p))
  (:default-initargs :clear-color (vec 0.1 0.1 0.1)
                     :context '(:vsync T)))

(defun launch (&rest args)
  (apply #'trial:launch 'workbench args))

(define-pool workbench)

(define-asset (workbench sphere) mesh
    (make-sphere-mesh 0.15 :segments 16))

(define-shader-entity globe (single-material-renderable transformed-entity)
  ((vertex-array :initform (// 'workbench 'sphere))))

(define-asset (workbench grassy-field) trial::environment-map
    #p"~/Projects/cl/trial-assets/data/grassy-field.hdr")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'skybox :texture (// 'workbench 'grassy-field :environment-map)) scene)
    (enter (make-instance 'fps-counter) scene)
    (enter (make-instance 'editor-camera :location (vec 0 0 3.0) :fov 50 :move-speed 0.1) scene)
    (enter (make-instance 'trial::environment-light :asset (asset 'workbench 'grassy-field)) scene)
    (loop for (p c) in `((,(vec -10  10 10) ,(vec 300 300 300))
                         (,(vec  10  10 10) ,(vec 300 300 300))
                         (,(vec -10 -10 10) ,(vec 300 300 300))
                         (,(vec  10 -10 10) ,(vec 300 300 300)))
          do (enter (make-instance 'point-light :location p :color c) scene))
    (loop for y from 0 below 7
          for m = (/ y 7.0)
          do (loop for x from 0 below 7
                   for r = (/ x 7.0)
                   for p = (vec (* 2.5 (- r 0.5)) (* 2.5 (- m 0.5)) 0.0)
                   for mat = (make-instance 'pbr-material :albedo-texture (// 'trial 'white) :metal-rough-occlusion-texture (// 'trial 'white)
                                                          :albedo-factor (vec 0.5 0 0 1) :metallic-factor m :roughness-factor r :occlusion-factor 0.0)
                   do (enter (make-instance 'globe :location p :material mat) scene)))
    (enter (make-instance 'pbr-render-pass) scene))
  (maybe-reload-scene))
