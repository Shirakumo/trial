#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defconstant +negative-infinity+
  #+sbcl sb-ext:single-float-negative-infinity
  #+clozure (coerce -infinity$$ 'single-float)
  #+abcl ext:single-float-negative-infinity
  #+allegro excl::*negative-infinity-single*
  #+cmu ext:single-float-negative-infinity
  #+(and ecl (not infinity-not-available)) si:single-float-negative-infinity
  #+lispworks (coerce -infinity$$ 'single-float)
  #+scl ext:single-float-negative-infinity
  #+t most-negative-single-float)

(defconstant +positive-infinity+
  #+sbcl sb-ext:single-float-positive-infinity
  #+clozure (coerce infinity$$ 'single-float)
  #+abcl ext:single-float-positive-infinity
  #+allegro excl::*infinity-single*
  #+cmu ext:single-float-positive-infinity
  #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
  #+lispworks (coerce infinity$$ 'single-float)
  #+scl ext:single-float-positive-infinity
  #+t most-positive-single-float)

(defun vangle (a b &optional c)
  "Calculates the angle between vectors A and B.
If C is provided, then A, B, and C are points where B->A and B->C are the vectors."
  (let ((vec (if c (v- a b) a))
        (other (if c (v- c b) b)))
    (atan (- (* (vx vec) (vy other))
             (* (vy vec) (vx other)))
          (+ (* (vx vec) (vx other))
             (* (vy vec) (vy other))))))
