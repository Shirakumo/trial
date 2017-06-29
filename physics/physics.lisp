#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial)
(defpackage #:trial-physics
  (:nicknames #:org.shirakumo.fraf.trial.physics)
  (:shadow #:scene #:entity #:load #:update)
  (:use #:cl #:3d-vectors #:3d-matrices #:flare #:trial)
  (:export #:physical-entity #:mass #:static-p #:forces))
(in-package #:org.shirakumo.fraf.trial.physics)

(defvar *default-forces* (vec 0 0.5)
  "Downways.")

(defclass physical-entity (located-entity rotated-entity pivoted-entity)
  ((mass :initarg :mass :accessor mass)
   (static-p :initarg :static-p :accessor static-p)
   (forces :initarg :forces :accessor forces))
  (:default-initargs :mass 1.0
                     :static-p NIL
                     :forces *default-forces*))
