#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defconstant +negative-infinity+
  #+sbcl sb-ext:single-float-negative-infinity
  #+abcl ext:single-float-negative-infinity
  #+allegro excl::*negative-infinity-single*
  #+clozure (coerce #.(unwind-protect
                           (progn
                             (ccl:set-fpu-mode :division-by-zero nil)
                             (/ -0d0))
                        (ccl:set-fpu-mode :division-by-zero t))
                    'single-float)
  #+cmu ext:single-float-negative-infinity
  #+(and ecl (not infinity-not-available)) si:single-float-negative-infinity
  #+lispworks (coerce #.(read-from-string "-10E999") 'single-float)
  #+scl ext:single-float-negative-infinity
  #+t cl:most-negative-single-float)

(defconstant +positive-infinity+
  #+sbcl sb-ext:single-float-positive-infinity
  #+abcl ext:single-float-positive-infinity
  #+allegro excl::*infinity-single*
  #+cmu ext:single-float-positive-infinity
  #+clozure (coerce #.(unwind-protect
                           (progn
                             (ccl:set-fpu-mode :division-by-zero nil)
                             (/ 0d0))
                        (ccl:set-fpu-mode :division-by-zero t))
                    'single-float)
  #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
  #+lispworks (coerce #.(read-from-string "10E999") 'single-float)
  #+scl ext:single-float-positive-infinity
  #+t cl:most-positive-single-float)

(defun vangle (a b &optional c)
  "Calculates the angle between vectors A and B where both vectors start from the origin.
If C is provided, then A, B, and C are points where B->A and B->C are the vectors."
  (let ((vector (if c (v- a b) a))
        (other (if c (v- c b) b)))
    (atan (vz (vc vector other))
          (v. vector other))))

(defun triangulate (point-a dist-a point-b dist-b point-c dist-c)
  "Calculates the location in a 2D plane based on three other points and their distances from the location."
  (declare (type vec point-a point-b point-c))
  (declare (type number dist-a dist-b dist-c))
  (when (v= point-a point-b) (error "POINT-A and POINT-B are equal"))
  (when (v= point-a point-c) (error "POINT-A and POINT-C are equal"))
  (when (v= point-b point-c) (error "POINT-B and POINT-C are equal"))
  (let* ((vec-ab (v- point-a point-b))
         (dist (vlength vec-ab))
         (a (/ (+ (* dist-a dist-a) (- (* dist-b dist-b)) (* dist dist)) (* 2 dist)))
         (h2 (- (* dist-a dist-a) (* a a)))
         (h (* (sqrt (abs h2)) (if (< 0 h2) -1 1)))
         (point-c (vec (vx point-c) (vy point-c)))
         (point-mid (v+ point-a (v* vec-ab a (/ dist))))
         (center-1 (vec2 (+ (vx point-mid)
                            (* h (- (vy point-b) (vy point-a)) (/ dist)))
                         (- (vy point-mid)
                            (* h (- (vx point-b) (vx point-a)) (/ dist)))))
         (center-2 (vec2 (- (vx point-mid)
                            (* h (- (vy point-b) (vy point-a)) (/ dist)))
                         (+ (vy point-mid)
                            (* h (- (vx point-b) (vx point-a)) (/ dist)))))
         (dist-1 (abs (vlength (v- point-c center-1))))
         (dist-2 (abs (vlength (v- point-c center-2)))))
    (if (< (abs (- (abs dist-c) dist-1)) (abs (- (abs dist-c) dist-2)))
        center-1 center-2)))

(defun ensure-vector-type (vector vector-type &optional (default (vec4 0 0 0 0)))
  (declare (type vec vector default))
  (declare (type symbol vector-type))
  (unless (or (eql vector-type 'vec2)
              (eql vector-type 'vec3)
              (eql vector-type 'vec4))
    (error "Invalid VECTOR-TYPE."))
  (if (typep vector vector-type)
      vector
      (case vector-type
        (vec2 (vec (vx vector) (vy vector)))
        (vec3 (vec (vx vector) (vy vector)
                   (if (vec4-p vector) (vz vector) (vz default))))
        (vec4 (vec (vx vector) (vy vector)
                   (if (vec3-p vector) (vz vector) (vz default))
                   (vw default))))))
