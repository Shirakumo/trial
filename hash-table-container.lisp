#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass hash-table-container (container)
  ((%objects :initform () :accessor %objects)))

(defmethod clear ((container hash-table-container))
  (let ((objects (%objects container)))
    (loop for object being the hash-keys of objects
          do (setf (container object) NIL))
    (clrhash objects))
  container)

(defmethod enter (thing (container hash-table-container))
  (setf (gethash thing (%objects container)) thing)
  thing)

(defmethod leave (thing (container hash-table-container))
  (remhash thing (%objects container))
  thing)

(defmethod sequences:elt ((container hash-table-container) index)
  (loop for i from 0
        for key being the hash-keys of (%objects container)
        do (when (= index i) (return key))
        finally (error "Index ~s is out of bounds." index)))

(defmethod (setf sequences:elt) (thing (container hash-table-container) index)
  (loop for i from 0
        for key being the hash-keys of (%objects container)
        do (when (= index i)
             (remhash key (%objects container))
             (setf (gethash thing (%objects container)) thing)
             (return thing))
        finally (error "Index ~s is out of bounds." index)))

(defstruct hash-table-iterator
  (index 0 :type (unsigned-byte 32))
  (donep NIL)
  (key NIL)
  (fun NIL))

(defmethod sequences:make-sequence-iterator ((container hash-table-container) &key (start 0) end from-end)
  (when from-end
    (error "Cannot iterate backwards."))
  (let ((state (with-hash-table-iterator (iterator (%objects container))
                 (make-hash-table-iterator
                  :fun (lambda () (iterator)))))
        (limit (or end most-positive-fixnum)))
    (flet ((update (state)
             (multiple-value-bind (valid key) (funcall (hash-table-iterator-fun state))
               (incf (hash-table-iterator-index state))
               (setf (hash-table-iterator-donep state) (or (not valid)
                                                           (<= limit (hash-table-iterator-index state))))
               (setf (hash-table-iterator-key state) key))))
      (dotimes (i start) (update state))
      (values state
              limit
              NIL
              ;; Next-state
              (lambda (container state from-end)
                (declare (ignore container from-end))
                (update state))
              ;; Endp
              (lambda (container state limit from-end)
                (declare (ignore container limit from-end))
                (hash-table-iterator-donep state))
              ;; Elt
              (lambda (container state)
                (declare (ignore container))
                (hash-table-iterator-key state))
              ;; Setf
              (lambda (value container state)
                (setf (gethash (hash-table-iterator-key state) (%objects container)) value))
              ;; Index
              (lambda (container state)
                (declare (ignore container))
                (hash-table-iterator-index state))
              ;; Copy
              (lambda (container state)
                (sequences:make-sequence-iterator container :start (hash-table-iterator-index state)))))))

(defmethod for:make-iterator ((container hash-table-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
