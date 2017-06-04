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
