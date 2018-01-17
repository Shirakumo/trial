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
  ;; physics.lisp
  (:export
   #:+negative-infinity+
   #:+positive-infinity+
   #:physical-entity
   #:mass
   #:static-p
   #:static-forces
   #:simulate
   #:quick-hull
   #:triangulate)
  ;; verlet.lisp
  (:export
   #:verlet-point
   #:verlet-entity
   #:verlet-simulation
   #:mass-points
   #:constraints)
  ;; constraint.lisp
  (:export
   #:distance-constraint
   #:pin-constraint
   #:angle-constraint
   #:frame-constraint))
