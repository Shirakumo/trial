#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass layer-container (container)
  ((layer :initarg :layer :accessor layer))
  (:default-initargs :layer 0))

(defclass layer-set (unit-container)
  ((objects :initform (make-array 0 :adjustable T :fill-pointer T))
   (index-map :initform (make-hash-table :test 'eql) :accessor index-map)))

(defmethod unit (index (layer-set layer-set))
  (gethash index (index-map layer-set)))

(defmethod enter ((n integer) (layer-set layer-set))
  (enter (or (unit n layer-set) (make-instance 'layer-container :layer n)) layer-set))

(defmethod enter ((layer layer-container) (layer-set layer-set))
  (when (unit (layer layer) layer-set)
    (cerror "A layer with index ~a already exists in ~a."
            (layer layer) layer-set))
  (vector-push-extend layer (objects layer-set))
  (setf (objects layer-set) (sort (objects layer-set) #'< :key #'layer))
  (setf (gethash (layer layer) (index-map layer-set)) layer))

(defmethod enter ((unit unit) (layer-set layer-set))
  (enter unit (unit 0 layer-set)))

(defmethod leave ((unit unit) (layer-set layer-set))
  (leave unit (unit 0 layer-set)))

(defmethod paint ((layer-set layer-set) target)
  (for:for ((layer across (objects layer-set)))
    (paint layer target)))

(defclass layered-unit (unit)
  ((layer :initarg :layer :accessor layer))
  (:default-initargs :layer 0))

(defmethod enter ((unit layered-unit) (layer-set layer-set))
  (enter unit (unit (layer unit) layer-set)))

(defmethod leave ((unit layered-unit) (layer-set layer-set))
  (leave unit (unit (layer unit) layer-set)))
