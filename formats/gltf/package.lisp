(defpackage #:org.shirakumo.fraf.trial.gltf
  (:use #:cl+trial)
  (:shadow #:asset #:load-image)
  (:local-nicknames
   (#:gltf #:org.shirakumo.fraf.gltf)
   (#:sequences #:org.shirakumo.trivial-extensible-sequences)
   (#:mem #:org.shirakumo.memory-regions)
   (#:v #:org.shirakumo.verbose))
  (:export
   #:translate-track-pointer
   #:translate-effect
   #:define-trigger-translation))
