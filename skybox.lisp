#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject skybox (textured-subject shader-subject)
  ()
  (:default-initargs
   :texture "skybox.png"
   :shader-program '(skybox :shaders ("skybox.vert" "skybox.frag"))))

(defmethod (setf texture) (thing (skybox skybox))
  (setf (texture skybox) (asset thing 'texture :target :texture-cube-map)))

(defmethod (setf texture) ((args list) (skybox skybox))
  (setf (texture skybox) (apply #'asset (first args) 'texture :target :texture-cube-map (rest args))))

(defmethod paint ((skybox skybox) target)
  (gl:disable :depth-test)
  (with-primitives :quads
    (gl:vertex -1 -1)
    (gl:vertex  1 -1)
    (gl:vertex  1  1)
    (gl:vertex -1  1))
  (gl:enable :depth-test))
