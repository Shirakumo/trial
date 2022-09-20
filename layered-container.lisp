#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass layered-container (container)
  ((objects :initform NIL))
  (:default-initargs :layer-count (error "LAYER-COUNT required.")))

(defmethod initialize-instance :after ((container layered-container) &key layer-count)
  (let ((objects (make-array layer-count)))
    (dotimes (i (length objects))
      (setf (aref objects i) (flare-indexed-set:make-indexed-set)))
    (setf (objects container) objects)))

(defgeneric layer-index (unit))

(defmethod layer-index ((_ unit)) 0)

(defmethod layer-count ((container layered-container))
  (length (objects container)))

(defmethod enter (thing (container layered-container))
  (flare-indexed-set:set-add thing (aref (objects container) (max 0 (round (layer-index thing))))))

(defmethod leave (thing (container layered-container))
  (flare-indexed-set:set-remove thing (aref (objects container) (max 0 (round (layer-index thing))))))

(defmethod for:step-functions ((iterator layered-container))
  (let* ((layers (objects iterator))
         (idx 0) layer cell tail)
    (flet ((update ()
             (setf layer (aref layers idx))
             (setf cell (flare-queue:right (flare-queue::head layer)))
             (setf tail (flare-queue::tail layer))))
      (update)
      (values (lambda ()
                (prog1 (flare-queue:value cell)
                  (setf cell (flare-queue:right cell))))
              (lambda ()
                (loop while (eql cell tail)
                      do (incf idx)
                         (if (< idx (length layers))
                             (update)
                             (return NIL))
                      finally (return T)))
              (lambda (value)
                (declare (ignore value))
                (error "Not supported"))
              (lambda ())))))

(defmethod for:object ((container layered-container)) container)

(defmethod for:make-iterator ((container layered-container) &key) container)
