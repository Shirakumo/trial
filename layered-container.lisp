#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass layered-container (container)
  ((%objects :initform NIL :accessor %objects))
  (:default-initargs :layer-count (error "LAYER-COUNT required.")))

(defmethod initialize-instance :after ((container layered-container) &key layer-count)
  (let ((objects (make-array layer-count)))
    (dotimes (i (length objects))
      (setf (aref objects i) (make-instance 'bag :container container)))
    (setf (%objects container) objects)))

(defgeneric layer-index (unit))

(defmethod layer-index (unit) 0)

(defmethod layer-count ((container layered-container))
  (length (%objects container)))

(defmethod enter (thing (container layered-container))
  (flare-indexed-set:set-add thing (aref (objects container) (clamp 0 (round (layer-index thing))
                                                                    (1- (length (objects container)))))))

(defmethod leave (thing (container layered-container))
  (flare-indexed-set:set-remove thing (aref (objects container) (clamp 0 (round (layer-index thing))
                                                                       (1- (length (objects container)))))))

(defmethod for:step-functions ((iterator layered-container))
  (let* ((layers (%objects iterator))
         (layer-idx 0) (idx 0) layer size)
    (flet ((update ()
             (let ((bag (aref layers layer-idx)))
               (setf layer (%objects bag))
               (setf idx 0)
               (setf size (size bag)))))
      (update)
      (values (lambda ()
                (prog1 (aref layer idx)
                  (incf idx)))
              (lambda ()
                (loop while (= 0 size)
                      do (incf layer-idx)
                         (if (< layer-idx (length layers))
                             (update)
                             (return NIL))
                      finally (return T)))
              (lambda (value)
                (declare (ignore value))
                (error "Not supported"))
              (lambda ())))))

(defmethod for:object ((container layered-container)) container)

(defmethod for:make-iterator ((container layered-container) &key) container)
