#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial-glop
  (:nicknames #:org.shirakumo.fraf.trial.glop)
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))
