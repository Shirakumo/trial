#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial-sdl2
  (:nicknames #:org.shirakumo.fraf.trial.sdl2)
  (:use #:cl+trial #:3d-vectors)
  (:shadow #:context #:window #:monitor #:initargs)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))
