#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial-qt
  (:nicknames #:org.shirakumo.fraf.trial.qt)
  (:use #:cl+qt #:trial #:3d-vectors)
  (:shadow #:context)
  (:shadowing-import-from #:cl+qt #:finalize #:connect #:with-slots-bound #:with-all-slots-bound)
  (:shadowing-import-from #:trial #:load)
  (:export
   #:context))
