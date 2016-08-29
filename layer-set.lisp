#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass layer-container (container)
  ((layer :initarg :layer :accessor layer)
   (active :initarg :active :accessor active))
  (:default-initargs
   :layer 0
   :active T))

(defmethod paint :around ((layer layer-container) target)
  (when (active layer)
    (call-next-method)))

;; FIXME: should be a container-unit, but things get recursive in the HUD
;;        if that is the case. BAD!
(defclass layer-set (container unit)
  ((objects :initform (make-array 0 :adjustable T :fill-pointer T))
   (index-map :initform (make-hash-table :test 'eql) :accessor index-map)))

(defmethod unit (index (layer-set layer-set))
  (or (gethash index (index-map layer-set))
      (enter (make-instance 'layer-container :layer index) layer-set)))

(defmethod enter ((layer layer-container) (layer-set layer-set))
  (when (gethash (layer layer) (index-map layer-set))
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

(defmethod layer-active-p (n (layer-set layer-set))
  (active (unit n layer-set)))

(defmethod (setf layer-active-p) (bool n (layer-set layer-set))
  (setf (active (unit n layer-set)) bool))

(defclass layered-unit (unit)
  ((layer :initarg :layer :accessor layer))
  (:default-initargs :layer 0))

(defmethod enter ((unit layered-unit) (layer-set layer-set))
  (enter unit (unit (layer unit) layer-set)))

(defmethod leave ((unit layered-unit) (layer-set layer-set))
  (leave unit (unit (layer unit) layer-set)))
