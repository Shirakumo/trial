#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod paint (thing target))

(defmethod paint-with (target thing)
  (paint thing target))

(defmethod finalize ((set flare-indexed-set:indexed-set))
  (flare-indexed-set:map-set #'finalize set))

(defmethod finalize ((container container))
  (finalize (objects container)))

(defmethod call-with-translation (func target (vec vec3))
  (with-pushed-matrix ()
    (translate vec)
    (funcall func)))
