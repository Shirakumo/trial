#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.harmony)

#+linux
(trial::dont-deploy
 org.shirakumo.fraf.mixed.pulse.cffi::libpulse-simple
 org.shirakumo.fraf.mixed.pulse.cffi::libpulse
 org.shirakumo.fraf.mixed.alsa.cffi::libasound)
#+darwin
(trial::dont-deploy
 org.shirakumo.fraf.mixed.coreaudio.cffi::audio-toolbox
 org.shirakumo.fraf.mixed.coreaudio.cffi::audio-unit)
#+windows
(trial::dont-deploy
 org.shirakumo.fraf.mixed.winmm.cffi::winmm
 org.shirakumo.fraf.mixed.wasapi.cffi::avrt)
