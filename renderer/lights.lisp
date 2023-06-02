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
  (attenuation :vec2 :accessor attenuation)
  (spot-radius :vec2 :accessor spot-radius)
  (shadow-map :int :initform #xFFFF :accessor shadow-map))

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
  (setf (color target) (color light))
  (setf (shadow-map target) #xFFFF))

(defmethod 3ds:location ((light light))
  #.(vec 0 0 0))

(defmethod cast-shadows-p ((light light)) NIL)

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
  (setf (attenuation target) (vec (linear-attenuation light) (quadratic-attenuation light))))

(defmethod 3ds:bsize ((light located-light))
  #.(vec 0 0 0))

(defclass point-light (located-light)
  ((shadow-map :initform NIL :accessor shadow-map)
   (cast-shadows-p :initform NIL :initarg :cast-shadows-p :accessor cast-shadows-p)))

(defmethod transfer-to progn ((target standard-light) (light point-light))
  (setf (light-type target) 2)
  (when (shadow-map light) (setf (shadow-map target) (shadow-map light))))

(defclass directional-light (light)
  ((direction :initform (vec 0 -1 0) :reader direction)
   (shadow-map :initarg :shadow-map :initform NIL :accessor shadow-map)
   (cast-shadows-p :initform T :initarg :cast-shadows-p :accessor cast-shadows-p)))

(defmethod shared-initialize :after ((light directional-light) slots &key direction)
  (when direction (setf (direction light) direction)))

(defmethod (setf direction) ((direction vec3) (light directional-light))
  (nvunit (v<- (direction light) direction))
  direction)

(defmethod transfer-to progn ((target standard-light) (light directional-light))
  (setf (light-type target) 3)
  (setf (direction target) (direction light))
  (when (shadow-map light) (setf (shadow-map target) (shadow-map light))))

(defmethod 3ds:bsize ((light directional-light))
  #.(vec most-positive-single-float
         most-positive-single-float
         most-positive-single-float))

(defclass spot-light (directional-light located-light)
  ((inner-radius :initform 0.976296)
   (outer-radius :initform 0.95371693)
   (quadratic-attenuation :initform 0.0)))

(defmethod shared-initialize :after ((light spot-light) slots &key inner-radius outer-radius target)
  (when inner-radius (setf (inner-radius light) inner-radius))
  (when outer-radius (setf (outer-radius light) outer-radius))
  (when target (setf (target light) target)))

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
  (setf (spot-radius target) (vec (slot-value light 'inner-radius) (slot-value light 'outer-radius))))

(defmethod (setf target) ((location vec3) (light spot-light))
  (v<- (direction light) location)
  (nv- (direction light) (location light))
  (nvunit (direction light))
  location)

(defmethod (setf target) ((entity entity) (light spot-light))
  (setf (target light) (global-location entity))
  entity)
