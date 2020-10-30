#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.harmony)

(deploy:define-library org.shirakumo.fraf.mixed.coreaudio.cffi::audio-toolbox
  :dont-deploy T)

(deploy:define-library org.shirakumo.fraf.mixed.coreaudio.cffi::audio-unit
  :dont-deploy T)
