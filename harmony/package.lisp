#|
 This file is a part of trial
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.harmony
  (:use #:cl)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:harmony #:org.shirakumo.fraf.harmony)
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:v #:org.shirakumo.verbose))
  (:export
   #:sound
   #:voice
   #:environment
   #:music
   #:main
   #:server-initargs
   #:settings-main))
