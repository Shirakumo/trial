(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.trial.glfw
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context #:window #:monitor #:initargs)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))
