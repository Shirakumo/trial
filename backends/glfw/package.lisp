(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.trial.glfw
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context #:window #:monitor #:initargs)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:glfw #:org.shirakumo.fraf.glfw)
   (#:%glfw #:org.shirakumo.fraf.glfw.cffi))
  (:export
   #:context))

(pushnew :trial-glfw *features*)
