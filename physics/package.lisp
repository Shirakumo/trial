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
