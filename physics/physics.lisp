#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defgeneric simulate (entity delta &key forces))


(define-shader-entity physical-entity (located-entity rotated-entity pivoted-entity)
  ((mass :initarg :mass :accessor mass)
   (static-p :initarg :static-p :accessor static-p)
   (rotates-p  :initarg :rotates-p :accessor rotates-p)
   (static-forces :initarg :static-forces :accessor static-forces))
  (:default-initargs :mass 1.0
                     :static-p NIL
                     :rotates-p T
                     :static-forces NIL))

(defmethod simulate ((entity physical-entity) delta &key forces)
  (declare (ignore entity delta forces)))

;; Below this is the calculation for Convex Hull. BE WARNED! It currently works only in 2D!
;; TODO: Read http://thomasdiewald.com/blog/?p=1888 for using QuickHull algorithm for 3D

(defun ensure-point (point)
  (etypecase point
    ((or textured-vertex located-entity) (location point))
    (vec point)))

(defun ensure-list (list)
  (etypecase list
    (array (for:for ((value across list)
                     (l collecting value))))
    (list list)))

(defun min-max-axis (points &key (axis-f #'vx))
  "Finds the minimum and maximum point in the VEC axis."
  (let* ((min-point (ensure-point (first points)))
         (max-point min-point)
         (other ()))
    (for:for ((point-like in (rest points))
              (point = (ensure-point point-like))
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
    (for:for ((point-like in points) ;; Check winding via cross product of (B - A) x (C - A)
              (point = (ensure-point point-like))
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
      (declare (ignore right on-line))
      (when highest-left
        (append
         (when left (find-hull left near highest-left))
         (list highest-left)
         (when left (find-hull left highest-left far)))))))

(defun quick-hull (points)
  "Calculates the Convex Hull points using QuickHull algorithm."
  (when (<= 2 (length points))
    (let ((points (ensure-list points)))
      (multiple-value-bind (near far)
          (min-max-axis points)
        (multiple-value-bind (left right)
            (points-relative-to-line near far points)
          (append (list near) ;; Upper hull
                  (find-hull left near far)
                  (list far) ;; Lower hull
                  (find-hull right far near)))))))
