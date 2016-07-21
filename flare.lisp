#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defmethod paint (thing target))

(defmethod finalize ((set flare-indexed-set:indexed-set))
  (flare-indexed-set:map-set #'finalize set))

(defmethod finalize ((container container))
  (finalize (objects container)))

(defmethod call-with-translation (func target (vec vec))
  (with-pushed-matrix
    (gl:translate (vx vec) (vy vec) (vz vec))
    (funcall func)))

(defmethod save-form-objects ((container container))
  (for:for ((object over container)
            (form = (make-save-form object))
            (forms when form collecting form))))

(define-saved-slots clock clock running timescale)
