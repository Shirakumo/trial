(defpackage #:org.shirakumo.fraf.trial.harmony
  (:use #:cl)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:harmony #:org.shirakumo.fraf.harmony)
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:v #:org.shirakumo.verbose)
   (#:math #:org.shirakumo.fraf.math))
  (:export
   #:sound
   #:voice
   #:environment
   #:music
   #:try-audio-backend
   #:initialize-audio-backend
   #:main
   #:server-initargs
   #:settings-main))

(pushnew :trial-harmony *features*)
