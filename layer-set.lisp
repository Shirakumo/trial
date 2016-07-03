#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass layer (container)
  ((index :initarg :index :accessor index)
   (active :initarg :active :accessor active))
  (:default-initargs
   :index 0
   :active T))

(defclass layer-entity (entity)
  ((layer :initarg :layer :accessor layer))
  (:default-initargs
   :layer 0))

(defmethod (setf layer) :after (new-layer (entity layer-entity))
  (let ((collective (collective entity)))
    (when (typep collective 'layer-set)
      (leave entity collective)
      (enter entity collective))))

(defclass layer-set (collective)
  ((layer-map :initform (make-hash-table :test 'eql) :reader layer-map)
   (layer-seq :initform (list (make-instance 'layer :index 0)) :accessor layer-seq)
   (objects :initform NIL)))

(defmethod initialize-instance :after ((layer-set layer-set) &key)
  (setf (gethash 0 (layer-map layer-set)) (layer-seq layer-set)))

(defmethod nth-layer (n (layer-set layer-set))
  (or (gethash n (layer-map layer-set))
      (setf (nth-layer n layer-set) (make-instance 'layer))))

(defmethod (setf nth-layer) ((layer layer) (n integer) (layer-set layer-set))
  (setf (index layer) n)
  (setf (layer-seq layer-set) (insert-index layer (layer-seq layer-set)))
  (setf (gethash n (layer-map layer-set)) layer))

;; objects stub. Since this is incompatible with how the usual OBJECTS accessor
;; works and coercing it would be too expensive, we just stub it with the 0 layer.
(defmethod objects ((layer-set layer-set))
  (objects (gethash 0 (layer-map layer-set))))

(defmethod (setf objects) (value (layer-set layer-set))
  (setf (objects (gethash 0 (layer-map layer-set))) value))

(defmethod update ((layer-set layer-set))
  (mapc #'update (layer-seq layer-set)))

(defmethod paint ((layer-set layer-set) target)
  (mapc (lambda (a) (paint a target)) (layer-seq layer-set)))

(defmethod insert ((layer-set layer-set) &rest objects)
  (mapc (lambda (a) (insert a objects)) (layer-seq layer-set)))

(defmethod withdraw ((layer-set layer-set) &rest objects)
  (mapc (lambda (a) (withdraw a objects)) (layer-seq layer-set)))

(defmethod units ((layer-set layer-set))
  (loop for layer in (layer-seq layer-set)
        nconc (units layer)))

(defmethod enter ((unit unit) (layer-set layer-set))
  (insert (nth-layer 0 layer-set) unit))

(defmethod enter ((entity layer-entity) (layer-set layer-set))
  (insert (nth-layer (layer entity) layer-set) entity))

(defmethod leave ((unit unit) (layer-set layer-set))
  (withdraw (nth-layer 0 layer-set) unit))

(defmethod leave ((entity layer-entity) (layer-set layer-set))
  (withdraw (nth-layer (layer entity) layer-set) entity))
