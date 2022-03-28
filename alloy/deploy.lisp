#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(deploy:define-hook (:deploy alloy) (directory)
  (deploy:status 1 "Copying fonts")
  (deploy:copy-directory-tree (org.shirakumo.alloy.renderers.opengl.msdf:fontcache-default-directory)
                              (pathname-utils:subdirectory directory "pool" "font-cache")
                              :copy-root NIL))

#+linux
(trial::dont-deploy
 org.shirakumo.font-discovery::fontconfig)
#+darwin
(trial::dont-deploy
 org.shirakumo.font-discovery::coretext
 org.shirakumo.font-discovery::foundation)
#+windows
(trial::dont-deploy
 org.shirakumo.font-discovery::directwrite
 org.shirakumo.com-on.cffi::ole32)
