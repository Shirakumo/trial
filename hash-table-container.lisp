(in-package #:org.shirakumo.fraf.trial)

(defclass hash-table-container (container)
  ((%objects :initform (make-hash-table :test 'eql) :accessor %objects)))

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

(defmethod sequences:make-sequence-like ((container hash-table-container) length &key (initial-element NIL iep) (initial-contents NIL icp))
  (let ((sub (make-instance 'hash-table-container)))
    (cond (icp
           (for:for ((el :over initial-contents)
                     (i :from 0 :to length))
             (setf (gethash el (%objects sub)) el)))
          (iep
           (setf (gethash initial-element (%objects sub)) initial-element)))
    sub))

(defmethod sequences:adjust-sequence ((container hash-table-container) length &key (initial-element NIL iep) (initial-contents NIL icp))
  (let ((table (make-hash-table :test 'eql)))
    (for:for ((el :table-keys (%objects container))
              (i :from 0 :to length))
      (setf (gethash el table) el))
    (cond (icp
           (for:for ((el :over initial-contents)
                     (i :from 0 :to length))
             (setf (gethash el table) el)))
          (iep
           (setf (gethash initial-element table) initial-element)))
    (setf (%objects container) table))
  container)

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
