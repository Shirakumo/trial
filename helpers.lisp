#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass located-entity (entity)
  ((location :initarg :location :initform (vec 0 0 0) :accessor location)))

(define-saved-initargs located-entity location)

(defmethod paint :around ((obj located-entity) target)
  (with-pushed-matrix
    (translate (location obj))
    (call-next-method)))

(defclass oriented-entity (entity)
  ((orientation :initarg :orientation :initform (vec 1 0 0) :accessor orientation)
   (up :initarg :up :initform (vec 0 1 0) :accessor up)))

(define-saved-initargs oriented-entity orientation up)

(defmethod paint :around ((obj oriented-entity) target)
  (with-pushed-matrix
    (rotate (vc (up obj) (orientation obj))
            (* 180 (/ (acos (v. (up obj) (orientation obj))) PI)))
    (call-next-method)))

(defclass rotated-entity (entity)
  ((rotation :initarg :rotation :initform (vec 0 0 0) :accessor rotation)))

(define-saved-initargs rotated-entity rotation)

(defmethod paint :around ((obj rotated-entity) target)
  (with-pushed-matrix
    (rotate +vx+ (vx (rotation obj)))
    (rotate +vy+ (vy (rotation obj)))
    (rotate +vz+ (vz (rotation obj)))
    (call-next-method)))

(defclass axis-rotated-entity (entity)
  ((axis :initarg :axis :initform (vec 0 1 0) :accessor axis)
   (angle :initarg :angle :initform 0 :accessor angle)))

(define-saved-initargs axis-rotated-entity axis angle)

(defmethod paint :around ((obj axis-rotated-entity) target)
  (with-pushed-matrix
    (rotate (axis obj) (angle obj))
    (call-next-method)))

(defclass pivoted-entity (entity)
  ((pivot :initarg :pivot :initform (vec 0 0 0) :accessor pivot)))

(define-saved-initargs pivoted-entity pivot)

(defmethod paint :around ((obj pivoted-entity) target)
  (with-pushed-matrix
    (translate (pivot obj))
    (call-next-method)))

(defclass bound-entity (located-entity)
  ((bounds :initarg :bounds :initform (vec 0 0 0) :accessor bounds)))

(define-saved-initargs bound-entity bounds)

(defmethod min-bound ((entity bound-entity) &key relative)
  (if relative (vec 0 0 0) (location entity)))

(defmethod max-bound ((entity bound-entity) &key relative)
  (v+ (if relative (vec 0 0 0) (location entity)) (bounds entity)))

(defmethod contains ((this bound-entity) (other bound-entity))
  (and (v<= (min-bound this) (min-bound other)) (v<= (max-bound other) (max-bound this))))

(defmethod intersects ((this bound-entity) (other bound-entity) &key ignore-y)
  (let* ((this-min (min-bound this))
         (other-min (min-bound other))
         (this-max (max-bound this))
         (other-max (max-bound other)))
    (when ignore-y
      (setf (vy this-min) 0
            (vy other-min) 0
            (vy this-max) 1
            (vy other-max) 1))
    ;; TODO: Make this better. It's essentially the same thing twice.
    (cond ((and (v<= this-min other-min) (v<= other-min this-max))
           (make-instance 'intersection-entity
                          :location other-min
                          :bounds (v+ other-min this-max)
                          :first-object this
                          :second-object other))
          ((and (v<= other-min this-min) (v<= this-min other-max))
           (make-instance 'intersection-entity
                          :location this-min
                          :bounds (v+ this-min other-max)
                          :first-object other
                          :second-object this)))))

(defclass intersection-entity (bound-entity)
  ((first-object :initarg :first-object :accessor first-object)
   (second-object :initarg :second-object :accessor second-object)
   (distance :initarg :distance :accessor distance)
   (normal :initarg :normal :accessor normal)
   (ray :initarg :ray :accessor ray))
  (:default-initargs
   :distance most-positive-single-float
   :normal (vec 0 0 0)))

(defclass collidable-entity (bound-entity)
  ((old-location :initform NIL :accessor old-location)))

(defmethod handle-collision ((entity collidable-entity) intersection))

;; FIXME for OGL3
(defclass face-entity (textured-entity bound-entity)
  ((tex-location :initarg :tex-location :initform (vec 0 0 0) :accessor tex-location)
   (tex-bounds :initarg :tex-bounds :initform (vec 1 1 0) :accessor tex-bounds)))

(defmethod paint ((entity face-entity) target)
  (with-slots ((tl tex-location) (tb tex-bounds) bounds) entity
    (with-pushed-attribs T
      (gl:disable :cull-face)
      (with-primitives :quads
        (gl:tex-coord (vx tl) (vy tl))
        (gl:vertex 0 0)
        (gl:tex-coord (vx tb) (vy tl))
        (gl:vertex (vx bounds) 0)
        (gl:tex-coord (vx tb) (vy tb))
        (gl:vertex (vx bounds) (vy bounds))
        (gl:tex-coord (vx tl) (vy tb))
        (gl:vertex 0 (vy bounds))))))
