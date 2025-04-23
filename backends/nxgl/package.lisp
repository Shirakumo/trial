(defpackage #:org.shirakumo.fraf.trial.nxgl
  (:use #:cl+trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context #:initargs)
  (:shadowing-import-from #:trial #:load)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:nxgl #:org.shirakumo.fraf.nxgl))
  (:export
   #:context
   #:show-keyboard
   #:performance-mode
   #:with-performance-mode))

(pushnew :trial-nxgl *features*)
