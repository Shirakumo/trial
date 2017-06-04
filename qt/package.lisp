#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial-qt
  (:nicknames #:org.shirakumo.fraf.trial.qt)
  (:use #:cl+qt #:trial)
  (:shadow #:context)
  (:shadowing-import-from #:flare #:slot)
  (:export
   #:context))
