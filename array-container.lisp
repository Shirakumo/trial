(in-package #:org.shirakumo.fraf.trial)

(defclass array-container (container)
  ((%objects :initform (make-array 0 :adjustable T :fill-pointer T) :accessor %objects)))

(defmethod clear ((container array-container))
  (let ((objects (%objects container)))
    (loop for i from 0 below (length objects)
          do (setf (container (aref objects i)) NIL)
             (setf (aref objects i) NIL))
    (adjust-array objects 0 :fill-pointer 0))
  container)

(defmethod enter (thing (container array-container))
  (vector-push-extend thing (%objects container))
  thing)

(defmethod leave (thing (container array-container))
  (let ((position (position thing (%objects container))))
    (when (null position)
      (error "The entity~%  ~a~%cannot be left from~%  ~a~%since the entity is not contained in the container.~
              ~@[~% It is contained in~%  ~a~]"
             thing container (container thing)))
    (array-utils:vector-pop-position (%objects container) position))
  thing)

(defmethod finalize ((container array-container))
  (for:for ((object across (%objects container)))
    (finalize object)))

(defmethod sequences:length ((container array-container))
  (length (%objects container)))

(defmethod sequences:elt ((container array-container) index)
  (aref (%objects container) index))

(defmethod (setf sequences:elt) (thing (container array-container) index)
  (setf (aref (%objects container) index) thing))

(defmethod sequences:make-sequence-like ((container array-container) length &rest args &key (initial-element NIL iep) (initial-contents NIL icp) &allow-other-keys)
  (declare (ignore initial-element initial-contents))
  (let ((sub (make-instance 'array-container)))
    (setf (%objects sub) (apply #'make-array length :adjustable T :fill-pointer T args))
    (unless (or iep icp)
      (replace (%objects sub) (%objects container)))
    sub))

(defmethod sequences:adjust-sequence ((container array-container) length &rest args &key &allow-other-keys)
  (setf (%objects container) (apply #'sequences:adjust-sequence (%objects container) length args))
  container)

(defstruct array-iterator
  (index 0 :type (unsigned-byte 32))
  (tindex 0 :type (unsigned-byte 32))
  (last-accessed NIL :type T))

(defmethod sequences:make-sequence-iterator ((container array-container) &key (start 0) end from-end)
  (let* ((end (or end (length (%objects container))))
         (index (if from-end (1- end) start))
         (iterator (make-array-iterator :index index :tindex index)))
    (macrolet ((arr (seq)
                 `(the (and vector (not simple-vector)) (%objects ,seq))))
      (values iterator
              (if from-end (1- start) end)
              from-end
              (lambda (sequence iterator from-end)
                (let ((dir (if from-end -1 +1)))
                  (when (eq (array-iterator-last-accessed iterator)
                            (aref (arr sequence) (array-iterator-tindex iterator)))
                    (incf (array-iterator-tindex iterator) dir))
                  (incf (array-iterator-index iterator) dir))
                iterator)
              (lambda (sequence iterator limit from-end)
                (declare (ignore sequence from-end))
                (= (array-iterator-index iterator) limit))
              (lambda (sequence iterator)
                (setf (array-iterator-last-accessed iterator) (aref (arr sequence) (array-iterator-tindex iterator))))
              (lambda (new-value sequence iterator)
                (setf (aref (arr sequence) (array-iterator-tindex iterator)) new-value))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                (array-iterator-index iterator))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                (copy-array-iterator iterator))))))

(defmethod for:make-iterator ((container array-container) &rest args)
  (apply #'for:make-iterator (%objects container) args))
