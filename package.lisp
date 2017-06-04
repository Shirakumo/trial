#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trial
  (:nicknames #:org.shirakumo.fraf.trial)
  (:use #:cl+qt #:3d-vectors #:3d-matrices #:flare)
  (:shadowing-import-from #:flare #:slot)
  (:shadow #:scene #:entity #:load)
  )

(flet ((reexport (in from)
         (do-external-symbols (symb in)
           (export (find-symbol (symbol-name symb) from) from))))
  (reexport '#:3d-vectors '#:trial)
  (reexport '#:flare '#:trial))

(defpackage #:trial-user
  (:nicknames #:org.shirakumo.fraf.trial.user)
  (:use #:cl+qt #:trial)
  (:shadowing-import-from #:trial #:load)
  (:shadowing-import-from #:flare #:slot))
