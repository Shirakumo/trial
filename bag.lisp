(in-package #:org.shirakumo.fraf.trial)

(defclass bag (container sequences:sequence)
  ((%objects :initform (make-array 0) :accessor %objects)
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
          (setf (gethash temp table) index)))
      (remhash thing table)
      (setf (size bag) last))
    thing))

(defmethod clear ((bag bag))
  (let ((objects (%objects bag)))
    (clrhash (%object->index bag))
    (loop for i from 0 below (size bag)
          do (when (aref objects i)
               (setf (container (aref objects i)) NIL)
               (setf (aref objects i) NIL)))
    (setf (size bag) 0)))

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

(defmethod sequences:make-sequence-like ((container bag) length &rest args &key (initial-element NIL iep) (initial-contents NIL icp) &allow-other-keys)
  (declare (ignore initial-element initial-contents))
  (let ((sub (make-instance 'bag)))
    (setf (%objects sub) (apply #'sequences:make-sequence-like (%objects container) length args))
    (unless (or iep icp)
      (replace (%objects sub) (%objects container)))
    (loop for object across (%objects sub)
          for i from 0 below length
          do (setf (gethash object (%object->index sub)) i))
    (setf (size sub) length)
    sub))

(defmethod sequences:adjust-sequence ((container bag) length &rest args &key initial-element initial-contents &allow-other-keys)
  (declare (ignore initial-element initial-contents))
  (let ((sub (make-instance 'bag)))
    (setf (%objects sub) (apply #'sequences:adjust-sequence (%objects container) length args))
    (clrhash (%object->index sub))
    (loop for object across (%objects sub)
          for i from 0 below length
          do (setf (gethash object (%object->index sub)) i))
    (setf (size sub) length)
    sub))

(defmethod sequences:make-sequence-iterator ((bag bag) &key (start 0) end from-end)
  (let* ((vector (the simple-vector (%objects bag)))
         (end (or end (size bag)))
         (iterator (if from-end (1- end) start))
         (limit (if from-end (1- start) end))
         (last-accessed NIL)
         (offset 0))
    ;; NOTE: the offset parameter is used to account for offsetting caused by
    ;;       removing the current element during iteration. It will not protect
    ;;       properly against removing multiple elements.
    (values iterator limit from-end
            (if from-end
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (unless (eq last-accessed (aref vector (+ iterator offset)))
                    (incf offset))
                  (1- iterator))
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (unless (eq last-accessed (aref vector (+ iterator offset)))
                    (decf offset))
                  (1+ iterator)))
            (lambda (sequence iterator limit from-end)
              (declare (ignore sequence from-end))
              (= iterator limit))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (setf last-accessed (aref vector (+ iterator offset))))
            (lambda (new-value sequence iterator)
              (declare (ignore sequence))
              (setf (aref vector (+ iterator offset)) new-value))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator)
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator))))

(defmethod for:step-functions ((bag bag))
  (multiple-value-bind (it limit from-end step endp elt setf) (sequences:make-sequence-iterator bag)
    (values (lambda ()
              (prog1 (funcall elt bag it)
                (setf it (funcall step bag it from-end))))
            (lambda ()
              (null (funcall endp bag it limit from-end)))
            (lambda (new)
              (funcall setf new bag it))
            (lambda ()))))

(defmethod for:make-iterator ((bag bag) &key) bag)
