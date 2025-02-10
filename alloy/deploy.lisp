(in-package #:org.shirakumo.fraf.trial.alloy)

(deploy:define-hook (:deploy alloy) (directory)
  (deploy:status 1 "Copying fonts")
  (deploy:copy-directory-tree (org.shirakumo.alloy.renderers.opengl.msdf:fontcache-default-directory)
                              (pathname-utils:subdirectory directory "pool" "font-cache")
                              :copy-root NIL)
  (deploy:status 1 "Caching Alloy shaders in-memory")
  (org.shirakumo.alloy.renderers.opengl::cache-shader-source-files)
  (setf (fdefinition 'org.shirakumo.alloy.renderers.opengl::shader-source)
        #'org.shirakumo.alloy.renderers.opengl::shader-source-from-cache))

(deploy:define-hook (:boot alloy) (directory)
  (setf org.shirakumo.alloy.renderers.opengl::*shaders-directory*
        (pathname-utils:subdirectory directory "pool" "alloy")))
