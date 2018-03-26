#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial)
(defpackage #:trial-physics
  (:nicknames #:org.shirakumo.fraf.trial.physics)
  (:shadow #:scene #:entity #:load #:update)
  (:use #:cl #:3d-vectors #:3d-matrices #:trial)
  (:export
   ;; math.lisp
   #:+negative-infinity+
   #:+positive-infinity+
   #:vangle
   #:triangulate
   #:ensure-vector-type)
  (:export
   ;; physics.lisp
   #:physical-entity
   #:mass
   #:static-p
   #:static-forces
   #:simulate
   #:quick-hull)
  (:export
   ;; verlet.lisp
   #:verlet-point
   #:verlet-entity
   #:verlet-simulation
   #:mass-points
   #:constraints)
  (:export
   ;; constraint.lisp
   #:distance-constraint
   #:pin-constraint
   #:angle-constraint
   #:frame-constraint))
