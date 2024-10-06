(in-package #:cl-user)
(defpackage #:trial-sdl2
  (:nicknames #:org.shirakumo.fraf.trial.sdl2)
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context #:window #:monitor #:initargs)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))

(pushnew :trial-sdl2 *features*)
