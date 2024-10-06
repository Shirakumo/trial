(in-package #:cl-user)
(defpackage #:trial-glop
  (:nicknames #:org.shirakumo.fraf.trial.glop)
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))

(pushnew :trial-glob *features*)
