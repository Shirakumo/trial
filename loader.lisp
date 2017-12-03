#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defgeneric compute-assets (object))
(defgeneric bake (bakable))
(defgeneric baked-p (bakable))
(defgeneric transition (from to))

(defmethod compute-assets ((null null))
  ())

(defmethod compute-assets ((cons cons))
  (nconc (compute-assets (car cons))
         (compute-assets (cdr cons))))

(defmethod compute-assets ((vector vector))
  (reduce #'nconc vector :key #'compute-assets))

(defmethod compute-assets ((table hash-table))
  (loop for value being the hash-values of table
        nconc (compute-assets value)))

(defmethod compute-assets ((struct structure-object))
  (loop for slot in (c2mop:class-slots (class-of struct))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp struct name)
        nconc (compute-assets (slot-value struct name))))

(defmethod compute-assets ((object standard-object))
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp object name)
        nconc (compute-assets (slot-value object (c2mop:slot-definition-name slot)))))

(defmethod compute-assets ((asset asset))
  (nconc (list asset) (call-next-method)))

(defclass bakable ()
  ((baked-p :initform NIL :accessor baked-p)))

(defmethod compute-assets :before ((bakable bakable))
  (bake bakable))

(defmethod bake :around ((bakable bakable))
  (unless (baked-p bakable)
    (call-next-method))
  (setf (baked-p bakable) T))

(defmethod transition ((from scene) (to scene))
  (v:info :trial.loader "Transitioning from ~a to ~a" from to)
  (let* ((from (compute-assets from))
         (to (compute-assets to))
         (to-load (set-difference to from))
         (to-offload (set-difference from to)))
    (v:info :trial.loader "Loading ~a assets." (length to-load))
    (mapc #'load to-load)
    (v:info :trial.loader "Offloading ~a assets." (length to-offload))
    (mapc #'offload to-offload)
    to))
