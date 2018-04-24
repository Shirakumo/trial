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

(defun perpendicular (vec-a vec-b)
  ;; TODO: more than 2D?
  (let ((x (- (vy vec-a) (vy vec-b)))
        (y (- (vx vec-b) (vx vec-a))))
    (nv* (vec2 x y) (/ (sqrt (+ (* x x) (* y y)))))))

(defun intersect-circles (center-a range-a center-b range-b)
  "Gets the intersections of two *circles* (not spheres)."
  (declare (type vec2 center-a center-b))
  (declare (type number range-a range-b))
  (let ((dist (vdistance center-a center-b)))
    (if (or (< (+ range-a range-b) dist)            ;; Do not overlap
            (< dist (abs (- range-a range-b)))      ;; One contains the other
            (and (= dist 0.0) (= range-a range-b))) ;; They are the same
        (values NIL NIL) ;; We return NIL points
        ;; Get distances of dimensions from the first point
        (let* ((a (+ (- (* range-a range-a) (* range-b range-b))
                     (/ (* dist dist) (* 2 dist))))
               (h (sqrt (- (* range-a range-a) (* a a))))
               ;; Determine point on the line between centers perpendicular to intersects
               (diff (v- center-b center-a))
               (perp (nv+ (v* diff (/ a dist)) center-a))
               (h-by-dist (/ h dist)))
          ;; And then the two intersections
          (nv* diff h-by-dist)
          (values (vec2 (round-to (+ (vx perp) (vy diff)) 4)
                        (round-to (- (vy perp) (vx diff)) 4))
                  (vec2 (round-to (- (vx perp) (vy diff)) 4)
                        (round-to (+ (vy perp) (vx diff)) 4)))))))

(defun triangulate (point-a dist-a point-b dist-b point-c dist-c)
  "Calculates the location in a 2D plane based on three other points and their distances from the location."
  (declare (type vec point-a point-b point-c))
  (declare (type number dist-a dist-b dist-c))
  (when (v= point-a point-b) (error "POINT-A and POINT-B are equal"))
  (when (v= point-a point-c) (error "POINT-A and POINT-C are equal"))
  (when (v= point-b point-c) (error "POINT-B and POINT-C are equal"))
  (let* ((point-a (ensure-vector-type point-a 'vec2))
         (point-b (ensure-vector-type point-b 'vec2))
         (point-c (ensure-vector-type point-c 'vec2))
         (circles (list (cons point-a dist-a)
                        (cons point-b dist-b)
                        (cons point-c dist-c)))
         (points ())
         (center-offset NIL)
         (center NIL))
    (for:for ((i repeat 3)
              (circle-a = (nth (mod i 3) circles))
              (circle-b = (nth (mod (1+ i) 3) circles))
              (circle-c = (nth (mod (+ 2 i) 3) circles))
              ((isect-a isect-b) = (multiple-value-list
                                    (intersect-circles
                                     (car circle-a) (cdr circle-a)
                                     (car circle-b) (cdr circle-b)))))
      (when (and isect-a isect-b)
        (let* ((offset-a (- (vdistance isect-a (car circle-c)) (cdr circle-c)))
               (offset-b (- (vdistance isect-b (car circle-c)) (cdr circle-c)))
               (cross-points (list (cons isect-a offset-a)
                                   (cons isect-b offset-b))))
          (if points
              (nconc points cross-points)
              (setf points cross-points)))))
    (for:for ((point in points)
              (isect = (car point))
              (offset = (cdr point)))
      (when (or (null center) (< offset center-offset))
        (setf center-offset offset
              center isect)))
    center))

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

(defun round-to (number precision &optional (round-f #'round))
  (if (/= 0 number)
      (let ((div (expt 10 precision)))
        (coerce (/ (funcall round-f (* number div)) div) 'single-float))
      0.0))

(defun square-distance (vec-a vec-b)
  (declare (type vec vec-a vec-b))
  (let ((x (- (vx vec-a) (vx vec-b)))
        (y (- (vy vec-a) (vy vec-b)))
        (z (if (and (or (vec3-p vec-a) (vec4-p vec-a))
                    (or (vec3-p vec-b) (vec4-p vec-b)))
               (- (vz vec-a) (vz vec-b))
               0.0))
        (w (if (and (vec4-p vec-a) (vec4-p vec-b))
               (- (vw vec-a) (vw vec-b))
               0.0)))
    (+ (* x x) (* y y) (* z z) (* w w))))
