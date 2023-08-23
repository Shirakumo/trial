(in-package #:org.shirakumo.fraf.trial.alloy)

(deploy:define-hook (:deploy alloy) (directory)
  (deploy:status 1 "Copying fonts")
  (deploy:copy-directory-tree (org.shirakumo.alloy.renderers.opengl.msdf:fontcache-default-directory)
                              (pathname-utils:subdirectory directory "pool" "font-cache")
                              :copy-root NIL)
  (deploy:copy-directory-tree org.shirakumo.alloy.renderers.opengl::*shaders-directory*
                              (pathname-utils:subdirectory directory "pool" "alloy")
                              :copy-root NIL))

(deploy:define-hook (:boot alloy) (directory)
  (setf org.shirakumo.alloy.renderers.opengl::*shaders-directory*
        (pathname-utils:subdirectory directory "pool" "alloy")))

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
