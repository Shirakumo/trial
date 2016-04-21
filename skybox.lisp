#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass skybox (textured-entity shader-entity)
  ()
  (:default-initargs
   :texture '(trial skybox)
   :shader-program '(trial skybox)))

(defmethod paint ((skybox skybox) target)
  (gl:disable :depth-test)
  (with-primitives :quads
    (gl:vertex -1 -1)
    (gl:vertex  1 -1)
    (gl:vertex  1  1)
    (gl:vertex -1  1))
  (gl:enable :depth-test))
