(defpackage #:org.shirakumo.fraf.trial.particle-studio
  (:nicknames #:trial-particle-studio)
  (:use #:cl+trial)
  (:shadow #:launch #:main)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:alloy #:org.shirakumo.alloy)
   (#:trial-alloy #:org.shirakumo.fraf.trial.alloy)
   (#:assets #:org.shirakumo.fraf.trial.assets)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:v #:org.shirakumo.verbose))
  (:export #:launch))

(in-package #:org.shirakumo.fraf.trial.particle-studio)
