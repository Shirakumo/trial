#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defmethod finalize ((set flare-indexed-set:indexed-set))
  (flare-indexed-set:map-set #'finalize set))

(defmethod finalize ((container container))
  (finalize (objects container)))

(defmethod call-with-translation (func (target main) (vec vec))
  (with-pushed-matrix
    (gl:translate (vx vec) (vy vec) (vz vec))
    (funcall func)))
