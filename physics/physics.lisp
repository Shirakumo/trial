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
  (:export #:update-physics))
(in-package #:org.shirakumo.fraf.trial.physics)

(defvar *default-forces* (vec 0 0.5)
  "Downways.")

(3d-vectors::define-ofun vnormal (v)
  (declare (ftype (function (vec)) vnormal))
  (v/ v (etypecase v
          (vec2 (+ (vx v) (vy v)))
          (vec3 (+ (vx v) (vy v) (vz v)))
          (vec4 (+ (vx v) (vy v) (vz v) (vw v))))))

(3d-vectors::define-ofun nvnormal (v)
  (declare (ftype (function (vec)) nvnormal))
  (let ((normal (vnormal v)))
    (setf (vx v) (vx normal)
          (vy v) (vy normal))
    (typecase v
      (vec3 (setf (vz v) (vz normal)))
      (vec4 (setf (vz v) (vz normal)
                  (vw v) (vw normal))))))

(defclass physical-entity (located-entity rotated-entity pivoted-entity)
  ((mass :initarg :mass :accessor mass)
   (static-p :initarg :static-p :accessor static-p)
   (forces :initarg :forces :accessor forces))
  (:default-initargs :mass 1.0
                     :static-p NIL
                     :forces *default-forces*))
