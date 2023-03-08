(defpackage #:org.shirakumo.fraf.trial.release
  (:use #:cl)
  (:local-nicknames
   (#:putils #:org.shirakumo.pathname-utils)
   (#:zippy #:org.shirakumo.zippy)
   (#:attributes #:org.shirakumo.file-attributes))
  (:export
   #:config
   #:set-config
   #:configure)
  (:export
   #:build
   #:test)
  (:export
   #:release
   #:deploy
   #:bundle
   #:upload
   #:make))
