#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial-glop
  (:nicknames #:org.shirakumo.fraf.trial.glop)
  (:use #:cl #:trial)
  (:shadow #:context)
  (:shadowing-import-from #:flare #:slot)
  (:export
   #:context))
