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
  (flare-indexed-set:set-add thing (aref (objects container) (layer-index thing))))

(defmethod leave (thing (container layered-container))
  (flare-indexed-set:set-remove thing (aref (objects container) (layer-index thing))))

(defmethod enter* ((thing entity) (container layered-container))
  (let ((objects (objects container))
        (preceding NIL))
    (loop for index downfrom (layer-index thing)
          for set = (aref objects index)
          for head = (flare-queue::head set)
          for last = (flare-queue::left (flare-queue::tail set))
          do (loop until (or (eq last head)
                             (typep (flare-queue:value last) 'renderable))
                   do (setf last (flare-queue::left last)))
             (unless (eq last head)
               (return (setf preceding (flare-queue:value last)))))
    (enter thing container)
    (compile-into-pass thing preceding *scene*)))

(defclass layered-container-iterator (for:iterator)
  ())

(defmethod for:step-functions ((iterator layered-container-iterator))
  (let* ((layers (for:object iterator))
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
                (error "Not supported"))
              (lambda ())))))

(defmethod for:make-iterator ((container layered-container) &key)
  (make-instance 'layered-container-iterator :object (objects container)))
