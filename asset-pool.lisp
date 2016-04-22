#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-pool :trial
  :base (asdf:system-relative-pathname :trial "data/"))

(define-asset font debug-hud (trial)
  :family "Consolas, Monospace"
  :size 12)

(define-asset texture cat (trial)
  :file "cat.png")

(define-asset texture skybox (trial)
  :file "skybox.png"
  :target :texture-cube-map)

(define-asset shader skybox-vert (trial)
  :file "skybox.vert")

(define-asset shader skybox-frag (trial)
  :file "skybox.frag")

(define-asset shader-program skybox (trial)
  :shaders '((trial skybox-vert)
             (trial skybox-frag)))
