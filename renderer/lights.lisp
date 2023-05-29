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
  (inner-radius :float :accessor inner-radius)
  (outer-radius :float :accessor outer-radius))

(defmethod active-p ((light standard-light))
  (< 0 (light-type light)))

(define-gl-struct standard-light-block
  (size NIL :initarg :size :initform 128 :reader size)
  (light-count :int :accessor light-count)
  (lights (:array (:struct standard-light) size) :reader lights))

(defclass light (entity)
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
  ((linear-attenuation :initarg :linear-attenuation :initform 0.0 :accessor linear-attenuation)
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

(defclass directional-light (light)
  ((direction :initform (vec 0 -1 0) :reader direction)))

(defmethod shared-initialize :after ((light directional-light) slots &key direction)
  (when direction (setf (direction light) direction)))

(defmethod (setf direction) ((direction vec3) (light directional-light))
  (nvunit (v<- (direction light) direction))
  direction)

(defmethod transfer-to progn ((target standard-light) (light directional-light))
  (setf (light-type target) 3)
  (setf (direction target) (direction light)))

(defmethod 3ds:bsize ((light directional-light))
  #.(vec most-positive-single-float
         most-positive-single-float
         most-positive-single-float))

(defclass spot-light (directional-light located-light)
  ((inner-radius :initform 0.976296)
   (outer-radius :initform 0.95371693)
   (quadratic-attenuation :initform 0.0)))

(defmethod shared-initialize :after ((light spot-light) slots &key inner-radius outer-radius)
  (when inner-radius (setf (inner-radius light) inner-radius))
  (when outer-radius (setf (outer-radius light) outer-radius)))

(defmethod inner-radius ((light spot-light))
  (rad->deg (acos (slot-value light 'inner-radius))))

(defmethod (setf inner-radius) (value (light spot-light))
  (setf (slot-value light 'inner-radius) (float (cos (deg->rad (float value 0f0))) 0f0))
  value)

(defmethod outer-radius ((light spot-light))
  (rad->deg (acos (slot-value light 'outer-radius))))

(defmethod (setf outer-radius) (value (light spot-light))
  (setf (slot-value light 'outer-radius) (float (cos (deg->rad (float value 0f0))) 0f0))
  value)

(defmethod transfer-to progn ((target standard-light) (light spot-light))
  (setf (light-type target) 4)
  (setf (inner-radius target) (slot-value light 'inner-radius))
  (setf (outer-radius target) (slot-value light 'outer-radius)))
