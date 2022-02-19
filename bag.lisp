#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass bag (container sequences:sequence)
  ((%objects :initform #() :accessor %objects)
   (%object->index :initform (make-hash-table :test 'eq) :accessor %object->index)
   (%count :initform 0 :accessor size :reader sequences:length)))

(defmethod enter (thing (bag bag))
  (let ((objects (%objects bag))
        (table (%object->index bag))
        (count (size bag)))
    (unless (gethash thing table)
      (setf (gethash thing table) count)
      (when (<= (length objects) count)
        (setf (%objects bag) (setf objects (adjust-array objects (* 2 (max 1 (length objects)))))))
      (setf (size bag) (1+ count))
      (setf (svref objects count) thing))
    thing))

(defmethod leave (thing (bag bag))
  (let* ((objects (%objects bag))
         (table (%object->index bag))
         (last (1- (size bag)))
         (index (gethash thing table)))
    (when index
      (when (< index last)
        (let ((temp (svref objects last)))
          (setf (svref objects index) temp)
          (setf (svref objects last) NIL)
          (setf (gethash table temp) index)))
      (remhash thing table)
      (setf (size bag) last))
    thing))

(defmethod clear ((bag bag))
  (let ((objects (%objects bag)))
    (clrhash (%object->index bag))
    (loop for i from 0 below (size bag)
          do (setf (aref objects i) NIL))
    (setf (size bag) 0)))

(defmethod contains-p (thing (bag bag))
  (gethash thing (%object->index bag)))

(defmethod sequences:elt ((bag bag) index)
  (svref (%objects bag) index))

(defmethod (setf sequences:elt) (thing (bag bag) index)
  (let* ((objects (%objects bag))
         (table (%object->index bag))
         (oldidx (gethash thing table))
         (temp (svref objects index)))
    (cond ((eq temp thing))
          (oldidx
           (setf (svref objects index) thing)
           (setf (svref objects oldidx) temp)
           (setf (gethash thing table) index)
           (setf (gethash temp table) oldidx))
          (T
           (setf (svref objects index) thing)
           (setf (gethash thing table) index)
           (remhash temp table)))))

(defmethod sequences:make-sequence-iterator ((bag bag) &key start end from-end)
  (let ((objects (%objects bag)))
    (values 0
            (size bag)
            from-end
            (lambda (seq state from-end) (1+ state))
            (lambda (seq state limit from-end) (< state limit))
            (lambda (seq state) (svref objects state))
            (lambda (value seq state))
            (lambda (seq state) state)
            (lambda (seq state) state))))

(defmethod for:step-functions ((iterator bag))
  (let ((objects (%objects iterator))
        (size (size iterator))
        (idx 0))
    (values (lambda ()
              (prog1 (aref objects idx)
                (incf idx)))
            (lambda ()
              (< idx size))
            (lambda (value)
              (declare (ignore value))
              (error "Not supported"))
            (lambda ()))))

(defmethod for:make-iterator ((bag bag) &key) bag)
