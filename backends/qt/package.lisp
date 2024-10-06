(in-package #:cl-user)
(defpackage #:trial-qt
  (:nicknames #:org.shirakumo.fraf.trial.qt)
  (:use #:cl+qt #:trial #:org.shirakumo.fraf.math.vectors)
  (:shadow #:context)
  (:shadowing-import-from #:cl+qt #:finalize #:connect #:with-slots-bound #:with-all-slots-bound #:ensure-class)
  (:shadowing-import-from #:trial #:load #://)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:export
   #:context))

(pushnew :trial-qt *features*)
