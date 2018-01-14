#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defgeneric simulate (entity delta &key forces))

(defvar *default-forces* (list (vec 0 0.05 0))
  "Directional forces affecting the physical entities.")

(defmethod 2d-frame-constraint ((entity located-entity)
                                &key (min (vec 0 0)) max)
  (let ((min-x (vx min))
        (min-y (vy min))
        (max-x (when max (vx max)))
        (max-y (when max (vy max))))
    #'(lambda ()
        (setf (vx (location entity)) (min (if max-x max-x (width *context*))
                                          (max min-x (vx (location entity))))
              (vy (location entity)) (min (if max-y max-y (height *context*))
                                          (max min-y (vy (location entity))))))))

(define-shader-entity physical-entity (located-entity rotated-entity pivoted-entity)
  ((mass :initarg :mass :accessor mass)
   (static-p :initarg :static-p :accessor static-p)
   (rotates-p  :initarg :rotates-p :accessor rotates-p)
   (constraints :initform (make-hash-table) :accessor constraints)
   (static-forces :initarg :static-forces :accessor static-forces))
  (:default-initargs :mass 1.0
                     :static-p NIL
                     :rotates-p T
                     :static-forces *default-forces*))

(defmethod add-constraint ((entity physical-entity) name constraint-f)
  (setf (gethash name (constraints entity)) constraint-f))

(defmethod constraint ((entity physical-entity) name)
  (gethash name (constraints entity)))

(defmethod constraints-as-list ((entity physical-entity))
  (alexandria:hash-table-values (constraints entity)))


(defmethod simulate ((entity physical-entity) delta &key forces)
  (declare (ignore entity delta forces)))

;; Below this is the calculation for Convex Hull. BE WARNED! It currently works only in 2D!
;; TODO: Read http://thomasdiewald.com/blog/?p=1888 for using QuickHull algorithm for 3D

(defun min-max-axis (points &key (axis-f #'vx))
  "Finds the minimum and maximum point in the VEC axis."
  (let* ((min-point (first points))
         (max-point (first points))
         (other ()))
    (for:for ((point in (rest points))
              (x = (funcall axis-f point)))
      (cond
        ((< x (funcall axis-f min-point))
         (when (v/= max-point min-point)
           (push min-point other))
         (setf min-point point))
        ((< (funcall axis-f max-point) x)
         (when (v/= max-point min-point)
           (push max-point other))
         (setf max-point point))))
    (values min-point max-point other)))

(defun points-relative-to-line (start end points)
  "Lists the points that are to the left of the line, right of the line, on the line, and the maximum distance points for left and right."
  (declare (type vec3 start end))
  (let ((left-list ())
        (right-list ())
        (on-line-list ())
        (highest-left NIL)
        (highest-right NIL)
        (highest-left-z -1)
        (highest-right-z -1)
        (delta (v- end start)))
    (for:for ((point in points) ;; Check winding via cross product of (B - A) x (C - A)
              (z = (vz (vc delta (v- point start)))))
      (cond
        ((< 0 z)
         (push point left-list)
         (when (< highest-left-z z)
           (setf highest-left-z z
                 highest-left point)))
        ((< z 0)
         (push point right-list)
         (when (< highest-right-z (abs z))
           (setf highest-right-z (abs z)
                 highest-right point)))
        ((and (v/= point start) (v/= point end))
         (push point on-line-list))))
    (values left-list right-list on-line-list
            highest-left highest-right)))

(defun find-hull (points near far)
  "A helper function for quick-hull"
  (when (and points near far)
    (multiple-value-bind (left right on-line highest-left)
        (points-relative-to-line near far points)
      (declare (ignore on-line))
      (declare (ignore right))
      (when highest-left
        (append
         (when left (find-hull left near highest-left))
         (list highest-left)
         (when left (find-hull left highest-left far)))))))

(defun quick-hull (points)
  "Calculates the Convex Hull points using QuickHull algorithm."
  (when (<= 2 (length points))
    (multiple-value-bind (near far)
        (min-max-axis points)
      (multiple-value-bind (left right)
          (points-relative-to-line near far points)
        (append (list near) ;; Upper hull
                (find-hull left near far)
                (list far) ;; Lower hull
                (find-hull right far near))))))

(defun triangulate (point-a dist-a point-b dist-b point-c dist-c)
  "Calculates the location in a 2D plane based on three other points and their distances from the location."
  (when (v= point-a point-b) (error "POINT-A and POINT-B are equal"))
  (when (v= point-a point-c) (error "POINT-A and POINT-C are equal"))
  (when (v= point-b point-c) (error "POINT-B and POINT-C are equal"))
  (let* ((vec-ab (v- point-a point-b))
         (dist (vlength vec-ab))
         (a (/ (+ (* dist-a dist-a) (- (* dist-b dist-b)) (* dist dist)) (* 2 dist)))
         (h (sqrt (- (* dist-a dist-a) (* a a))))
         (point-c (vec (vx point-c) (vy point-c)))
         (point-mid (v+ point-a (v* vec-ab a (/ dist))))
         (center-1 (vec (+ (vx point-mid)
                           (* h (- (vy point-b) (vy point-a)) (/ dist)))
                        (- (vy point-mid)
                           (* h (- (vx point-b) (vx point-a)) (/ dist)))))
         (center-2 (vec (- (vx point-mid)
                           (* h (- (vy point-b) (vy point-a)) (/ dist)))
                        (+ (vy point-mid)
                           (* h (- (vx point-b) (vx point-a)) (/ dist)))))
         (dist-1 (abs (vlength (v- point-c center-1))))
         (dist-2 (abs (vlength (v- point-c center-2)))))
    (if (< (abs (- (abs dist-c) dist-1)) (abs (- (abs dist-c) dist-2)))
        center-1 center-2)))
