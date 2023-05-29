#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct standard-light
  (type :int :accessor light-type)
  (position :vec3 :accessor location)
  (direction :vec3 :accessor direction)
  (color :vec3 :accessor color)
  (linear-attenuation :float :accessor linear-attenuation)
  (quadratic-attenuation :float :accessor quadratic-attenuation)
  (outer-radius :float :accessor outer-radius)
  (cutoff-radius :float :accessor cutoff-radius))

(defmethod active-p ((light standard-light))
  (< 0 (light-type light)))

(define-gl-struct standard-light-block
  (size NIL :initarg :size :initform 128 :reader size)
  (light-count :int :accessor light-count)
  (lights (:array (:struct standard-light) size) :reader lights))

(defclass light ()
  ((color :initarg :color :initform (vec 1 1 1) :accessor color)
   (active-p :initarg :active-p :initform T :accessor active-p)))

(defmethod transfer-to progn ((target standard-light) (light light))
  (setf (color target) (color light)))

(defmethod 3ds:location ((light light))
  #.(vec 0 0 0))

(defclass ambient-light (light)
  ())

(defmethod transfer-to progn ((target standard-light) (light ambient-light))
  (setf (light-type target) 1)
  (setf (color target) (color light)))

(defmethod 3ds:bsize ((light ambient-light))
  #.(vec most-positive-single-float
         most-positive-single-float
         most-positive-single-float))

(defclass located-light (light located-entity)
  ((linear-attenuation :initarg :linear-attenuation :initform 1.0 :accessor linear-attenuation)
   (quadratic-attenuation :initarg :quadratic-attenuation :initform 1.0 :accessor quadratic-attenuation)))

(defmethod transfer-to progn ((target standard-light) (light located-light))
  (setf (location target) (global-location light))
  (setf (linear-attenuation target) (linear-attenuation light))
  (setf (quadratic-attenuation target) (quadratic-attenuation light)))

(defmethod 3ds:bsize ((light located-light))
  #.(vec 0 0 0))

(defclass point-light (located-light)
  ())

(defmethod transfer-to progn ((target standard-light) (light point-light))
  (setf (light-type target) 2))

(defclass directional-light (located-light)
  ((direction :initarg :direction :initform (vec 0 -1 1) :accessor direction)))

(defmethod transfer-to progn ((target standard-light) (light directional-light))
  (setf (light-type target) 3)
  (setf (direction target) (direction light)))

(defclass spot-light (directional-light)
  ((outer-radius :initarg :outer-radius :initform 32.0 :accessor outer-radius)
   (cutoff-radius :initarg :cutoff-radius :initform 32.0 :accessor cutoff-radius)))

(defmethod transfer-to progn ((target standard-light) (light spot-light))
  (setf (light-type target) 4)
  (setf (outer-radius target) (outer-radius light))
  (setf (cutoff-radius target) (cutoff-radius light)))
